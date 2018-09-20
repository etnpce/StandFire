import numpy as np
import gdal as G
from collections import namedtuple
from string import Template
from FuelModels import models
from without_gc import no_gc

from typing import Iterable, Generator, Dict, BinaryIO, Tuple

#  Resolution of lcp input, fixed by LandFire
RASTER_RES = 30
#  Minimum mesh space above the top of any tree, added to max tree height to get height above ground
MESH_OVERHEAD = 10

_Stand_Template = Template("\n"
                           "&SURF ID ='${stand_id}'\n"
                           "FREE_SLIP=.TRUE.\n"
                           "VEG_LEVEL_SET_SPREAD = .${spread}.\n"
                           "VEG_LSET_ELLIPSE=.TRUE.\n"
                           "VEG_LSET_SURFACE_FIRE_HEAD_ROS_MODEL= 'ROTHERMEL' \n"
                           "VEG_LSET_CROWN_FIRE_HEAD_ROS_MODEL= 'CRUZ' \n"
                           "VEG_LSET_ROTH_ZEROWINDSLOPE_ROS =  ${ros}\n"
                           "VEG_LSET_HEAT_OF_COMBUSTION=${heat}\n"
                           "VEG_LSET_BETA = ${packing_ratio}\n"
                           "VEG_LSET_SIGMA = ${sav}\n"
                           "VEG_LSET_SURF_HEIGHT =  ${depth}\n"
                           "VEG_LSET_SURF_LOAD   =  ${load}\n"
                           "VEG_LSET_CHAR_FRACTION = 0.2\n"  # Should I try to make this dynamic?
                           "VEG_LSET_SURF_EFFM = ${effm}\n"
                           "VEG_LSET_WAF_SHELTERED =  ${waf}\n"
                           "VEG_LSET_CANOPY_BULK_DENSITY= ${canopy_density}\n"
                           "VEG_LSET_FUEL_STRATA_GAP = ${canopy_base_gap}\n"
                           "VEG_LSET_CANOPY_HEIGHT= ${canopy_height}\n"
                           "VEG_LSET_CRUZ_PROB_CROWN=0.5\n"
                           "VEG_LSET_CROWNFIRE_ANGLE = 90\n"
                           "PART_ID='TE'\n"
                           "NPPC = 5\n"
                           "VEL = -0.01 \n"
                           "RGB=122,117,48 /\n")

_Vent_Template = Template("&VENT XB=$lx,$ux,$ly,$uy,$z,$z, SURF_ID='IGN FIRE' /\n")

_OBST_Template = Template("&OBST XB=$lx,$ux,$ly,$uy,0,$z,SURF_ID=''/\n"
                          "&OBST XB=$lx,$ux,$ly,$uy,$z,$z,SURF_ID='${stand_id}'/\n")


def prime_facts(n):
    # type: (int) -> list[int]
    res = []
    # optimize powers of two, as they are the most expected input
    while n & 1 == 0:
        res.append(2)
        n >>= 1
    d = 3
    while d * d <= n:
        divisor, remainder = divmod(n, d)
        while not remainder:
            res.append(d)
            n = divisor
            divisor, remainder = divmod(n, d)  # I don't think this duplication is avoidable
        d += 2
    if n != 1:
        res.append(n)
    return res


MultiMesh = namedtuple('MultiMesh', 'mesh_pos_y mesh_pos_x size_y size_x rep_y rep_x')
StandEntry = namedtuple('StandEntry', 'id output')


def _split_meshes(meshes, factor):
    # type: (Iterable[MultiMesh], int) -> Generator[MultiMesh]
    # Tuple Destructuring
    for (pos_y, pos_x, size_y, size_x, rep_y, rep_x) in meshes:
        if size_x > size_y:
            sub_size, excess = divmod(size_x, factor)
            sub_reps = rep_x * (factor - excess)
            yield MultiMesh(pos_y, pos_x, size_y, sub_size, rep_y, sub_reps)
            if excess > 0:
                yield MultiMesh(pos_y, pos_x + sub_size * sub_reps, size_y, sub_size + 1, rep_y, rep_x * excess)
        else:
            sub_size, excess = divmod(size_y, factor)
            sub_reps = rep_y * (factor - excess)
            yield MultiMesh(pos_y, pos_x, sub_size, size_x, sub_reps, rep_x)
            if excess > 0:
                yield MultiMesh(pos_y + sub_size * sub_reps, pos_x, sub_size + 1, size_x, rep_y * excess, rep_x)


"""def _interpolate_elevations(raster, raster_size_y, raster_size_x, ratio):
    # type: (np.ndarray, int, int, int) -> np.ndarray
    elevations = np.empty([raster_size_y * ratio, raster_size_x * ratio], dtype=np.int16, order='C')
    for y in xrange(raster_size_y):
        for x in xrange(raster_size_x):
            elevation, slope, aspect = raster[y, x, :3]
            for dy in xrange(ratio):
                for dx in xrange(ratio):
                    elevations[y * ratio + dy, x * ratio + dx] = elevation  # todo interpolate
    return elevations
"""


def _output_meshes(outfile, meshes, mesh_res, ratio, elevations, mesh_overhead):
    # type: (BinaryIO, Iterable[MultiMesh], int, int, np.ndarray, int) -> ()
    mesh_overhead = mesh_overhead + mesh_res - 1  # combine mesh_overhead with integer round up constant
    # Tuple Destructuring
    for (mesh_pos_y, mesh_pos_x, size_y, size_x, rep_y, rep_x) in meshes:
        # print(mesh_pos_y, mesh_pos_x, size_y, size_x, rep_y, rep_x)
        for pos_y in xrange(mesh_pos_y, mesh_pos_y + size_y * rep_y, size_y):
            for pos_x in xrange(mesh_pos_x, mesh_pos_x + size_x * rep_x, size_x):
                sub_elevation = elevations[pos_y // ratio: (pos_y + size_y + ratio - 1) // ratio,
                                           pos_x // ratio: (pos_x + size_x + ratio - 1) // ratio]
                # round min_z down and max_z up to be aligned to global mesh
                min_z = sub_elevation.min() // mesh_res - 1
                max_z = (sub_elevation.max() + mesh_overhead) // mesh_res
                outfile.write("&MESH IJK = {}, {}, {}, XB = {}, {}, {}, {}, {}, {} /\n".format(
                    size_x, size_y, max_z - min_z,  # Mesh dimensions
                                    pos_x * mesh_res, (pos_x + size_x) * mesh_res,  # Mesh X begin and end coordinates
                                    pos_y * mesh_res, (pos_y + size_y) * mesh_res,  # Y coordinates
                                    min_z * mesh_res, max_z * mesh_res))  # Z coordinates


def _stand_id_from(stand_cache, raster_cell):
    model_id, _, canopy_height, canopy_base_height, canopy_density = raster_cell[3:]
    return stand_cache[(model_id, canopy_height, canopy_base_height, canopy_density)].id


# mesh_res must divide {RASTER_RES} evenly
def gen(lower_vent_x, lower_vent_y, upper_vent_x, upper_vent_y, ignition_time, in_file="dat/lcp/us_140lcp40.lcp",
        out_file="./out/test.txt", run_title="TestLevelSet",
        mesh_res=10, n_meshes=1, time_span=300):
    with no_gc():  # I believe this helps reduce overhead of these massive allocations
        data = G.Open(in_file, G.GA_ReadOnly)  # Must not be Garbage Collected before raster
        # Canopy Height is in decimeters, round up to meters and then add fixed overhead
        mesh_overhead = (int(data.GetRasterBand(6).GetMetadataItem("CANOPY_HT_MAX")) + 9) // 10 + MESH_OVERHEAD
        raster_size_y = data.RasterYSize  # type: int
        raster_size_x = data.RasterXSize  # type: int
        raster = data.GetVirtualMemArray(band_sequential=False)  # type: np.ndarray
        elevations = raster[:, :, 0]  # Not sure if this is slower than getting it separately
        ratio = RASTER_RES // mesh_res  # Mesh cells per Raster cell
        assert mesh_res * ratio == RASTER_RES  # Check that mesh_res is a factor of RASTER_RES
        # Y/XBound are in mesh cells (not units or raster cells)
        bound_y = raster_size_y * ratio
        bound_x = raster_size_x * ratio
        # elevations = _interpolate_elevations(raster, raster_size_y, raster_size_x, ratio)
        meshes = (MultiMesh(0, 0, bound_y, bound_x, 1, 1),)  # Single element iterable/stream
        for factor in reversed(prime_facts(n_meshes)):  # Perform large splits first
            meshes = _split_meshes(meshes, factor)
        stands_cache = {}  # type: Dict[Tuple[np.int16, np.int16, np.int16, np.int16], StandEntry]
        prv_stand_id = 1
        for y in raster:
            for x in y:
                model_id, _, canopy_height, canopy_base_height, canopy_density = x[3:]
                k = (model_id, canopy_height, canopy_base_height, canopy_density)
                if k not in stands_cache:
                    model = models[int(model_id)]
                    prv_stand_id += 1
                    stand_id = '{}_{}'.format(model.model_id, prv_stand_id)  # prefix removed for smaller file
                    stands_cache[k] = StandEntry(stand_id,
                                                 _Stand_Template.substitute(
                                                     stand_id=stand_id,
                                                     spread=str(model.model_id[0:2] != 'NB').upper(),
                                                     ros=None,  # TODO
                                                     heat=model.heat_content,
                                                     packing_ratio=model.packing_ratio,
                                                     sav=model.avg_surface_volume_ratio,
                                                     depth=model.depth,
                                                     load=model.fuel_load,
                                                     effm=None,  # TODO ????
                                                     waf=None,  # TODO ????
                                                     canopy_density=canopy_density,
                                                     canopy_base_gap=canopy_base_height - model.depth,
                                                     canopy_height=canopy_height
                                                 ))
        with open(out_file, 'w') as output:
            def sep(section):
                print(section)
                output.write('\n---- {} ----\n'.format(section))

            output.write("&HEAD CHID='{0}', TITLE='{0}' /\n\n".format(run_title))
            sep('Meshes')
            _output_meshes(output, meshes, mesh_res, ratio, elevations, mesh_overhead)

            sep('Header')
            output.write(("&time T_END={} /\n"
                          "\n"
                          "&MISC   TERRAIN_CASE=.FALSE.\n"
                          "        VEG_LEVEL_SET_UNCOUPLED=.FALSE.\n"
                          "        VEG_LEVEL_SET_COUPLED=.TRUE.\n"
                          "        VEG_LEVEL_SET_SURFACE_HEATFLUX=.FALSE.\n"
                          "        VEG_LEVEL_SET_THERMAL_ELEMENTS=.TRUE.\n"
                          "        LAPSE_RATE=-0.0065 /\n"
                          "\n"
                          "&RADI RADIATION=.FALSE. /\n"
                          ).format(time_span))

            sep('Stand Definitions')
            output.writelines((stand.output for stand in stands_cache.values()))
            output.write("\n"
                         "&PART ID='TE',\n"
                         "AGE=50,\n"
                         "TE_BURNTIME=2.5,\n"
                         "MASSLESS=.TRUE.,\n"
                         "SAMPLING_FACTOR=50,\n"
                         "COLOR='BLACK' /\n\n"
                         "&SURF ID ='IGN FIRE',VEG_LSET_IGNITE_TIME={}, COLOR = 'RED' /\n".format(ignition_time))

            sep('Vents')
            output.writelines(_Vent_Template.substitute(
                lx=x * RASTER_RES, ux=(x + 1) * RASTER_RES, ly=y * RASTER_RES, uy=(y + 1) * RASTER_RES, z=elevations[y, x])
                              for x in xrange(lower_vent_x // RASTER_RES, (upper_vent_x + RASTER_RES - 1) // RASTER_RES)
                              for y in xrange(lower_vent_y // RASTER_RES, (upper_vent_y + RASTER_RES - 1) // RASTER_RES))

            sep('Ground and Stands')  # Need to simplify output greatly
            # need to know whether wfds levelset cares about stand z
            # The next line means a refactor will be required if wfds levelset dislikes obstacles going out of the mesh
            output.writelines(_OBST_Template.substitute(lx=x * RASTER_RES, ux=(x + 1) * RASTER_RES,
                                                        ly=y * RASTER_RES, uy=(y + 1) * RASTER_RES, z=elevations[y, x],
                                                        stand_id=_stand_id_from(stands_cache, raster[y, x]))
                              for x in xrange(0, raster_size_x) for y in xrange(0, raster_size_y))

            sep('Footer')
            output.write("&SURF ID='wind',RAMP_V='wind', PROFILE='ATMOSPHERIC', Z0=10., PLE=0.143, VEL=-4 /\n"
                         "&RAMP ID='wind',F=1,T=0 /\n"
                         "&RAMP ID='wind',F=1,T=1 /\n"
                         "&VENT MB = XMIN, SURF_ID = 'wind' /\n"
                         "&VENT MB = XMAX, SURF_ID = 'OPEN' /\n"
                         "&VENT MB = YMIN, SURF_ID = 'MIRROR' /\n"
                         "&VENT MB = YMAX, SURF_ID = 'MIRROR' /\n"
                         "&VENT MB = ZMAX, SURF_ID = 'OPEN' /\n"
                         "-- Outputs\n"
                         "&DUMP DT_SLCF=1,DT_BNDF=1,DT_ISOF=10,DT_PL3D=200,SMOKE3D=.FALSE. /\n"
                         "&SLCF PBY=150,QUANTITY='VELOCITY',VECTOR=.TRUE. /\n"
                         "&SLCF PBY=150,QUANTITY='TEMPERATURE' /\n"
                         "&ISOF QUANTITY='TEMPERATURE',VALUE=50. /\n"
                         "&BNDF QUANTITY='WALL TEMPERATURE' /\n"
                         "&SLCF PBZ=1,AGL_SLICE=1,QUANTITY='VELOCITY',VECTOR=.TRUE. / \n"
                         "&SLCF PBZ=1,AGL_SLICE=10,QUANTITY='VELOCITY',VECTOR=.TRUE. / \n"
                         "-- END of Input file\n"
                         "&TAIL /\n".format())


""" Canceled subdivision by 2 with 5,3 special cases
# noinspection SpellCheckingInspection
def gen_meshes(ypos, xpos, ysize, xsize, nmeshes):
    assert nmeshes >= 1
    if nmeshes == 1:
        lz, uz = _calc_zbounds(ypos, xpos, ysize, xsize)
        output.write(
            "&MESH IJK = {}, {}, {}, XB = {}, {}, {}, {}, {}, {} /\n".format(
                xsize, ysize, uz - lz,  # Mesh dimensions
                xpos * MESH_RES, (xpos + xsize) * MESH_RES,  # Mesh X begin and end coordinates
                ypos * MESH_RES, (ypos + ysize) * MESH_RES,  # Y coordinates
                lz * MESH_RES, uz * MESH_RES))  # Z coordinates
    else:
        for factor in bigdivide_factors:
            sub_nmeshes, remainder = divmod(nmeshes, factor)
            if not remainder:
                if xsize > ysize:
                    sub_xsize, rem = divmod(xsize, factor)
                    for i in xrange(xpos, xpos+xsize, sub_xsize):
                        pass
                    for i in xrange(factor - rem):
                        pass
                else:
                    sub_ysize, rem = divmod(ysize, factor)
                    for i in xrange(rem):
                        pass
                    for i in xrange(factor - rem):
                        pass
                return
        # Partition into approximate halves for higher factors
        sub_nmeshes = nmeshes >> 1
        if xsize > ysize:
            sub_xsize = xsize >> 1
            gen_meshes(ypos, xpos, ysize, sub_xsize, sub_nmeshes)
            gen_meshes(ypos, xpos + sub_xsize, ysize, xsize - sub_xsize, nmeshes - sub_nmeshes)
        else:
            sub_ysize = ysize >> 1
            gen_meshes(ypos, xpos, sub_ysize, xsize, sub_nmeshes)
            gen_meshes(ypos + sub_ysize, xpos, ysize - sub_ysize, xsize, nmeshes - sub_nmeshes)
"""
