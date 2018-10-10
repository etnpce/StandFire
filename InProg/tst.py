import gdal as G
from collections import namedtuple
from enum import Enum
from itertools import count, izip
from math import sqrt
from string import Template

import numpy as np
from typing import Iterable, Generator, Dict, BinaryIO, Tuple

from FuelModels import models, MODEL_10_ROS, MT_PER_FT
from without_gc import no_gc

#  Resolution of lcp input, fixed by LandFire
RASTER_RES = 30
#  Minimum mesh space above the top of any tree, added to max tree height to get height above ground
MESH_OVERHEAD = 10


class CrownMode(Enum):
    CRUZ = "CRUZ"
    SR = "SR"


_Stand_Template = Template("\n"
                           "&SURF ID='${stand_id}'\n"
                           "FREE_SLIP=.TRUE.\n"
                           "VEG_LEVEL_SET_SPREAD=.${spread}.\n"
                           "VEG_LSET_ELLIPSE=.TRUE.\n"
                           "VEG_LSET_SURFACE_FIRE_HEAD_ROS_MODEL='ROTHERMEL' \n"
                           "VEG_LSET_ROTH_ZEROWINDSLOPE_ROS=${ros}\n"
                           "VEG_LSET_HEAT_OF_COMBUSTION=${heat}\n"
                           "VEG_LSET_BETA=${packing_ratio}\n"
                           "VEG_LSET_SIGMA=${sav}\n"
                           "VEG_LSET_SURF_HEIGHT=${depth}\n"
                           "VEG_LSET_SURF_LOAD=${load}\n"
                           "VEG_LSET_CHAR_FRACTION=0.2\n"  # Should I try to make this dynamic?
                           "${crown}"
                           "RGB=122,117,48 /\n")

_CRUZ_Template = Template("VEG_LSET_CROWN_FIRE_HEAD_ROS_MODEL='CRUZ'\n"
                          "VEG_LSET_SURF_EFFM=${effm}\n"  # FINE FUEL MOISTURE
                          "VEG_LSET_FUEL_STRATA_GAP=${canopy_base_gap}\n"
                          "VEG_LSET_CROWNFIRE_ANGLE=90\n"
                          "VEG_LSET_CRUZ_PROB_CROWN=0.5\n")

_SR_Template = Template("VEG_LSET_CROWN_FIRE_HEAD_ROS_MODEL='SR'\n"
                        "VEG_LSET_MODEL_FOR_PASSIVE_ROS = 'SR'\n"
                        "VEG_LSET_CANOPY_FMC=1\n"
                        "VEG_LSET_CANOPY_BASE_HEIGHT=${canopy_base_height}\n"
                        "VEG_LSET_ROTHFM10_ZEROWINDSLOPE_ROS=${other_ros}\n")

_COMMON_CROWN_Template = Template("VEG_LSET_WAF_${un}SHELTERED=${waf}\n"
                                  "VEG_LSET_CANOPY_BULK_DENSITY=${canopy_density}\n"
                                  "VEG_LSET_CANOPY_HEIGHT=${canopy_height}\n")


def _output_surf(outfile, stand, crown_mode, level_mode):
    # type: (BinaryIO, Tuple[Tuple[np.int16, np.int16, np.int16, np.int16, np.int16], str], CrownMode, int) -> ()
    model_id, canopy_cover, canopy_height, canopy_base_height, canopy_density = stand[0]
    model = models[int(model_id)]
    crown_str = ""
    if canopy_height > 0:
        if crown_mode is CrownMode.CRUZ:
            crown_str = _CRUZ_Template.substitute(effm=model.fine_fuel_moisture,
                                                  canopy_base_gap=canopy_base_height - model.depth)
        elif crown_mode is CrownMode.SR:
            crown_str = _SR_Template.substitute(other_ros=MODEL_10_ROS, canopy_base_height=canopy_base_height)
        if crown_mode in CrownMode:
            waf = model.waf
            crown_ratio = canopy_base_height / canopy_height
            f = canopy_cover / 3 * crown_ratio
            un = "UN"
            if f >= .05:
                un = ""
                waf = waf * .555 / 1.83 / sqrt(f * model.depth / MT_PER_FT)  # Behave docs
            crown_str += _COMMON_CROWN_Template.substitute(un=un, waf=waf, canopy_density=canopy_density,
                                                           canopy_height=canopy_height)
    if level_mode == 4:
        crown_str += ("PART_ID='TE'\n"
                      "NPPC=1\n"
                      "VEL=-0.01 \n")

    outfile.write(_Stand_Template.substitute(
        stand_id=stand[1],
        spread=str(model.model_id[0:2] != 'NB').upper(),
        ros=model.ros,
        heat=model.heat_content,
        packing_ratio=model.packing_ratio,
        sav=model.avg_surface_volume_ratio,
        depth=model.depth,
        load=model.fuel_load,
        crown=crown_str
    ))


_Vent_Template = Template("&VENT XB=$lx,$ux,$ly,$uy,$z,$z,SURF_ID='IGN FIRE'/\n")

_OBST_Template = Template("&OBST XB=$lx,$ux,$ly,$uy,0,$z,SURF_ID='${stand_id}'/\n")


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
                min_z = sub_elevation.min() // mesh_res - 1  # TODO See if this needs to always be 0
                max_z = (sub_elevation.max() + mesh_overhead) // mesh_res
                outfile.write("&MESH IJK = {}, {}, {}, XB = {}, {}, {}, {}, {}, {} /\n".format(
                              size_x, size_y, max_z - min_z,  # Mesh dimensions
                              pos_x * mesh_res, (pos_x + size_x) * mesh_res,  # Mesh X begin and end coordinates
                              pos_y * mesh_res, (pos_y + size_y) * mesh_res,  # Y coordinates
                              min_z * mesh_res, max_z * mesh_res))  # Z coordinates


# mesh_res must divide {RASTER_RES} evenly
def gen(levelset_mode, lower_vent_x, lower_vent_y, upper_vent_x, upper_vent_y, ignition_time,
        lcp_file="dat/lcp/us_140lcp40_Landscape_1.lcp", out_file="./out/test.txt", run_title="TestLevelSet",
        mesh_res=10, n_meshes=1, time_span=300):
    """
    :param levelset_mode: 1-4
    :param lower_vent_x: pos of left edge of ignition box
    :param lower_vent_y: pos of top edge of ignition box
    :param upper_vent_x: pos of right edge of ignition box
    :param upper_vent_y: pos of bottom edge of ignition box
    :param ignition_time: how long ignition box is lit
    :param lcp_file: where the .lcp file to generate with is
    :param out_file: where to dump the WFDS level set input file
    :param run_title: The title of the WFDS run
    :param mesh_res: resolution of the output mesh, must be a divisor of RASTER_RES = 30
    :param n_meshes: how many cores you plan to run the result with
    :param time_span: how long the WFDS input file simulates for
    """
    with no_gc():  # I believe this helps reduce overhead of these massive allocations
        data = G.Open(lcp_file, G.GA_ReadOnly)  # Must not be Garbage Collected before raster
        # Canopy Height is in decimeters, round up to meters and then add fixed overhead
        mesh_overhead = (int(data.GetRasterBand(6).GetMetadataItem("CANOPY_HT_MAX")) + 9) // 10 + MESH_OVERHEAD
        half_mesh_res = mesh_res >> 1
        raster_size_y = data.RasterYSize  # type: int
        raster_size_x = data.RasterXSize  # type: int
        raster_size_y = raster_size_x = 250  # TODO kill
        raster = data.GetVirtualMemArray(band_sequential=False)  # type: np.ndarray
        raster = raster[:raster_size_x, :raster_size_y, ::]  # TODO kill
        elevations = raster[:, :, 0]  # Not sure if this is slower than getting it separately
        raster = raster[:, :, 3:]  # Convenience, might gain speed
        ratio = RASTER_RES // mesh_res  # Mesh cells per Raster cell
        assert mesh_res * ratio == RASTER_RES  # Check that mesh_res is a factor of RASTER_RES
        # Y/XBound are in mesh cells (not units or raster cells)
        bound_y = raster_size_y * ratio
        bound_x = raster_size_x * ratio
        # elevations = _interpolate_elevations(raster, raster_size_y, raster_size_x, ratio)
        meshes = (MultiMesh(0, 0, bound_y, bound_x, 1, 1),)  # Single element iterable/stream
        for factor in reversed(prime_facts(n_meshes)):  # Perform large splits first
            meshes = _split_meshes(meshes, factor)

        # Dict comprehensions don't skip repeated keys
        stand_map = {k: models[int(k[0])].model_id + '_' + str(i)
                     for (k, i) in izip({tuple(x) for y in raster for x in y}, count(1))}
        # type: Dict[Tuple[np.int16, np.int16, np.int16, np.int16, np.int16], str]

        with open(out_file, 'w') as output:
            def sep(section):
                print(section)
                output.write('\n---- {} ----\n'.format(section))

            output.write("&HEAD CHID='{0}', TITLE='{0}' /\n\n".format(run_title))
            sep('Meshes')
            _output_meshes(output, meshes, mesh_res, ratio, elevations, mesh_overhead)

            # TODO Figure out lvlset 2 wind thing

            sep('Header')
            output.write(("&TIME T_END={time_span} /\n"
                          "\n"
                          "&MISC   TERRAIN_CASE=.FALSE.\n"
                          "        VEG_LEVEL_SET_UNCOUPLED=.{not_coupled}.\n"
                          "        VEG_LEVEL_SET_COUPLED=.{coupled}.\n"
                          "        VEG_LEVEL_SET_SURFACE_HEATFLUX=.FALSE.\n"
                          "        VEG_LEVEL_SET_THERMAL_ELEMENTS=.{thermal_elements}.\n"
                          "        WIND_ONLY=.{wind_only}.\n"
                          "{u0}"
                          "        PROJECTION=.TRUE. /\n"
                          # "        LAPSE_RATE=-0.0065 /\n"
                          "\n"
                          "&RADI RADIATION=.FALSE. /\n"
                          ).format(time_span=time_span,
                                   not_coupled=str(levelset_mode < 3).upper(),
                                   coupled=str(levelset_mode >= 3).upper(),
                                   thermal_elements=str(levelset_mode == 4).upper(),
                                   u0="" if levelset_mode != 1 else "        U0=" + str(11)+'\n',  # TODO init wind vel
                                   wind_only=str(False).upper()))  # TODO how to input this?

            sep('Stand Definitions')
            for stand in stand_map.items():
                # For some reason the type checker fails here, thinking CrownMode == str != CrownMode
                # noinspection PyTypeChecker
                _output_surf(output, stand, CrownMode.SR, levelset_mode)

            output.write("\n"
                         "&PART ID='TE',\n"
                         " AGE=50,\n"
                         " TE_BURNTIME=2.5,\n"
                         " MASSLESS=.TRUE.,\n"
                         " SAMPLING_FACTOR=50,\n"
                         " COLOR='BLACK' /\n"
                         "\n"
                         "&SURF ID ='IGN FIRE',VEG_LSET_IGNITE_TIME={}, COLOR = 'RED' /\n".format(ignition_time))

            sep('Vents')
            output.writelines(_Vent_Template.substitute(
                lx=x * RASTER_RES, ux=(x + 1) * RASTER_RES,
                ly=y * RASTER_RES, uy=(y + 1) * RASTER_RES,
                z=((elevations[y, x] + half_mesh_res) // mesh_res) * mesh_res)
                    for x in xrange(lower_vent_x // RASTER_RES, (upper_vent_x + RASTER_RES - 1) // RASTER_RES)
                    for y in xrange(lower_vent_y // RASTER_RES, (upper_vent_y + RASTER_RES - 1) // RASTER_RES))

            sep('Ground and Stands')

            founds = np.zeros([raster_size_y, raster_size_x], dtype=np.bool8, order='C')
            for y in xrange(0, raster_size_y):
                for x in xrange(0, raster_size_x):
                    if not founds[y, x]:
                        k = raster[y, x]
                        # round half-up to nearest mesh_res
                        elevation = ((elevations[y, x] + half_mesh_res) // mesh_res) * mesh_res
                        elevation_lower = elevation - half_mesh_res
                        elevation_upper = elevation + half_mesh_res + (mesh_res & 1)  # Correction for odd rounding
                        max_max_x = x + 1
                        assert elevation_lower <= elevation <= elevation_upper
                        while max_max_x < raster_size_x and not founds[y, max_max_x] \
                                and elevation_lower <= elevations[y, max_max_x] <= elevation_upper \
                                and np.array_equal(k, raster[y, max_max_x]):
                            max_max_x += 1
                        max_max_y = y + 1
                        max_size = max_max_x - x  # max_max_x is one too big, but subtracting x goes one too small
                        max_y = y + 1
                        prev_max_x = max_max_x
                        while max_y < raster_size_y and not founds[max_y, x] \
                                and elevation_lower <= elevations[max_y, x] <= elevation_upper \
                                and np.array_equal(k, raster[max_y, x]):
                            max_x = x + 1
                            while max_x < prev_max_x and not founds[max_y, max_x] \
                                    and elevation_lower <= elevations[max_y, max_x] <= elevation_upper \
                                    and np.array_equal(k, raster[max_y, max_x]):
                                max_x += 1
                            prev_max_x = max_x
                            max_y += 1  # Moved here so that max_y is one past valid for saving of max_max_y
                            size = (max_x - x) * (max_y - y)
                            if size > max_size:
                                max_size = size
                                max_max_x = max_x
                                max_max_y = max_y

                        for y2 in xrange(y, max_max_y):
                            for x2 in xrange(x, max_max_x):
                                founds[y2, x2] = True
                        output.write(_OBST_Template.substitute(lx=x * RASTER_RES, ux=max_max_x * RASTER_RES,
                                                               ly=y * RASTER_RES, uy=max_max_y * RASTER_RES,
                                                               z=elevation, stand_id=stand_map[tuple(k)]))

            """output.writelines(_OBST_Template.substitute(lx=x * RASTER_RES, ux=(x + 1) * RASTER_RES,
                                                        ly=y * RASTER_RES, uy=(y + 1) * RASTER_RES, z=elevations[y, x],
                                                        stand_id=stand_map[tuple(raster[y, x])])
                              for x in xrange(0, raster_size_x) for y in xrange(0, raster_size_y))"""

            sep('Footer')
            output.write("-&SURF ID='wind',RAMP_V='wind', PROFILE='ATMOSPHERIC', Z0=10., PLE=0.143, VEL=-4 /\n"
                         "-&RAMP ID='wind',F=1,T=0 /\n"
                         "-&RAMP ID='wind',F=1,T=1 /\n"
                         "-&VENT MB = XMIN, SURF_ID = 'wind' /\n"
                         "-&VENT MB = XMAX, SURF_ID = 'OPEN' /\n"
                         "-&VENT MB = YMIN, SURF_ID = 'MIRROR' /\n"
                         "-&VENT MB = YMAX, SURF_ID = 'MIRROR' /\n"
                         "-&VENT MB = ZMAX, SURF_ID = 'OPEN' /\n"
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
