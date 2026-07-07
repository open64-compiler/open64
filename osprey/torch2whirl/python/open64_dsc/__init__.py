"""Python ingestion skeleton for the Open64 torch2whirl frontend."""

from .export import export_to_whirl, save_as_whirl
from .module import WhirlModule
from .options import WhirlExportOptions

__all__ = [
    "WhirlExportOptions",
    "WhirlModule",
    "export_to_whirl",
    "save_as_whirl",
]
