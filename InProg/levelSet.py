from typing import Tuple

# Will require lcp file for now
class LevelSet(object):
    def __init__(self, lcp_file, out_file, run_name, mode, mesh_res, n_meshes, time_span, ignition_data):
        # type: (LevelSet, str, str, str, int, int, int, Tuple[int, int, int, int, int]) -> LevelSet
        assert 1 <= mode <= 4


    def write_Header(self):
        pass



    def write_Footer(self):
        pass

    # TODO ask how I should input and handle wind
# I'll come back to this