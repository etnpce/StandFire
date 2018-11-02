from typing import NamedTuple, Dict

class FuelClass(NamedTuple):
    load : float
    surface_volume_ratio : float
    moisture_content : float

class FuelModel(NamedTuple):
    model_id : str
    dead1hr : FuelClass
    dead10hr : FuelClass
    dead100hr : FuelClass
    herb : FuelClass
    wood : FuelClass
    dynamic : bool
    depth : float
    heat_content : float
    mineral_content : float
    effective_mineral_content : float
    particle_density : float
    extinction_moisture : float
    fuel_load : float
    avg_surface_volume_ratio : float
    packing_ratio : float
    fine_fuel_moisture : float
    waf : float
    ros : float
    color : str

models : Dict[int, FuelModel]

MODEL_10_ROS : float
MT_PER_FT : float