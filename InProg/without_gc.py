import gc
from typing import ContextManager


class _GCDisabler(ContextManager):
    def __enter__(self):
        gc.disable()

    def __exit__(self, exc_type, exc_val, exc_tb):
        gc.enable()
        return False


class _NoopManager(ContextManager):
    def __enter__(self):
        pass

    def __exit__(self, exc_type, exc_val, exc_tb):
        return False


_gc_disabler = _GCDisabler()
_noop_manager = _NoopManager()


def no_gc():
    # type: () -> ContextManager[None]
    return _gc_disabler if gc.isenabled() else _noop_manager

