from collections import namedtuple

FuelModel = namedtuple('FuelModel',
                       'model_id dead1hr dead10hr dead100hr herb wood dynamic depth heat_content '
                       'mineral_content effective_mineral_content particle_density '
                       'extinction_moisture fuel_load avg_surface_volume_ratio packing_ratio')
FuelClass = namedtuple('FuelClass', 'load surface_volume_ratio moisture_content')

MODEL_MOISTURE_D1HR = 6
MODEL_MOISTURE_D10HR = 7
MODEL_MOISTURE_D100HR = 8
MODEL_MOISTURE_HERB = 60
MODEL_MOISTURE_WOOD = 90
MODEL_SAV_D10HR = 109
MODEL_SAV_D100HR = 30
MODEL_HEAT_CONT = 8000
MODEL_MINERAL_CONTENT = 5.55
MODEL_EFFECTIVE_MINERAL_CONTENT = 1
MODEL_PARTICLE_DENSITY = 32

MT_PER_FT = 0.3048
KG_PER_LB = 0.453592
KJ_PER_BTU = 1.05506
KG_PER_TON = 2000 * KG_PER_LB
SQ_MT_PER_ACRE = MT_PER_FT * MT_PER_FT * 43560


# noinspection PyPep8Naming
def _make(model_id, loadD1hr, loadD10hr, loadD100hr, loadHerb, loadWood, dynamic, SAV_D1hr, SAV_herb, SAV_Wood, depth,
          extinctionMoisture, avg_SAV, packing_ratio):
    return FuelModel(model_id,
                     FuelClass(loadD1hr * KG_PER_TON / SQ_MT_PER_ACRE,
                               SAV_D1hr / MT_PER_FT,
                               MODEL_MOISTURE_D1HR / 100),
                     FuelClass(loadD10hr * KG_PER_TON / SQ_MT_PER_ACRE,
                               MODEL_SAV_D10HR / MT_PER_FT,
                               MODEL_MOISTURE_D10HR / 100),
                     FuelClass(loadD100hr * KG_PER_TON / SQ_MT_PER_ACRE,
                               MODEL_SAV_D100HR / MT_PER_FT,
                               MODEL_MOISTURE_D100HR / 100),
                     FuelClass(loadHerb * KG_PER_TON / SQ_MT_PER_ACRE,
                               SAV_herb / MT_PER_FT,
                               MODEL_MOISTURE_HERB / 100),  # NOTE: herb moisture should be interpolated somehow later
                     FuelClass(loadWood * KG_PER_TON / SQ_MT_PER_ACRE,
                               SAV_Wood / MT_PER_FT,
                               MODEL_MOISTURE_WOOD / 100),
                     dynamic,
                     depth * MT_PER_FT,
                     MODEL_HEAT_CONT * KJ_PER_BTU / KG_PER_LB,
                     MODEL_MINERAL_CONTENT / 100,
                     MODEL_EFFECTIVE_MINERAL_CONTENT / 100,
                     MODEL_PARTICLE_DENSITY * KG_PER_LB / MT_PER_FT ** 3,
                     extinctionMoisture / 100,
                     (loadD1hr + loadHerb + loadWood) * KG_PER_TON / SQ_MT_PER_ACRE,
                     avg_SAV / MT_PER_FT,
                     packing_ratio)


models = {
     91: _make('NB1', 0, 0, 0, 0, 0, False, -1, -1, -1, 0, 0, 0, 0),
     92: _make('NB2', 0, 0, 0, 0, 0, False, -1, -1, -1, 0, 0, 0, 0),
     93: _make('NB3', 0, 0, 0, 0, 0, False, -1, -1, -1, 0, 0, 0, 0),
     98: _make('NB8', 0, 0, 0, 0, 0, False, -1, -1, -1, 0, 0, 0, 0),
     99: _make('NB9', 0, 0, 0, 0, 0, False, -1, -1, -1, 0, 0, 0, 0),
    101: _make('GR1', 0.10, 0.00, 0.00, 0.30, 0.00, True, 2200, 2000, -1, 0.4, 15, 2054, .00143),
    102: _make('GR2', 0.10, 0.00, 0.00, 1.00, 0.00, True, 2000, 1800, -1, 1.0, 15, 1820, 0.00158),
    103: _make('GR3', 0.10, 0.40, 0.00, 1.50, 0.00, True, 1500, 1300, -1, 2.0, 30, 1290, 0.00143),
    104: _make('GR4', 0.25, 0.00, 0.00, 1.90, 0.00, True, 2000, 1800, -1, 2.0, 15, 1826,  0.00154),
    105: _make('GR5', 0.40, 0.00, 0.00, 2.50, 0.00, True, 1800, 1600, -1, 1.5, 40, 1631, 0.00277),
    106: _make('GR6', 0.10, 0.00, 0.00, 3.40, 0.00, True, 2200, 2000, -1, 1.5, 40, 2006,  0.00335),
    107: _make('GR7', 1.00, 0.00, 0.00, 5.40, 0.00, True, 2000, 1800, -1, 3.0, 15, 1834, 0.00306),
    108: _make('GR8', 0.50, 1.00, 0.00, 7.30, 0.00, True, 1500, 1300, -1, 4.0, 30, 1302, 0.00316),
    109: _make('GR9', 1.00, 1.00, 0.00, 9.00, 0.00, True, 1800, 1600, -1, 5.0, 40, 1612, 0.00316),
    121: _make('GS1', 0.20, 0.00, 0.00, 0.50, 0.65, True, 2000, 1800, 1800, 0.9, 15, 1832, 0.00215),
    122: _make('GS2', 0.50, 0.50, 0.00, 0.60, 1.00, True, 2000, 1800, 1800, 1.5, 15, 1827, 0.00249),
    123: _make('GS3', 0.30, 0.25, 0.00, 1.45, 1.25, True, 1800, 1600, 1600, 1.8, 40, 1614, 0.00259),
    124: _make('GS4', 1.90, 0.30, 0.10, 3.40, 7.10, True, 1800, 1600, 1600, 2.1, 40, 1674, 0.00874),
    141: _make('SH1', 0.25, 0.25, 0.00, 0.15, 1.30, True, 2000, 1800, 1600, 1.0, 15, 1674,  0.00280),
    142: _make('SH2', 1.35, 2.40, 0.75, 0.00, 3.85, False, 2000, -1, 1600, 1.0, 15, 1672, 0.01198),
    143: _make('SH3', 0.45, 3.00, 0.00, 0.00, 6.20, False, 1600, -1, 1400, 2.4, 40, 1371, 0.00577),
    144: _make('SH4', 0.85, 1.15, 0.20, 0.00, 2.55, False, 2000, 1800, 1600, 3.0, 30, 1682, 0.00227),
    145: _make('SH5', 3.60, 2.10, 0.00, 0.00, 2.90, False, 750, -1, 1600, 6.0, 15, 1252, 0.00206),
    146: _make('SH6', 2.90, 1.45, 0.00, 0.00, 1.40, False, 750, -1, 1600, 2.0, 30, 1144, 0.00412),
    147: _make('SH7', 3.50, 5.30, 2.20, 0.00, 3.40, False, 750, -1, 1600, 6.0, 15, 1233, 0.00344),
    148: _make('SH8', 2.05, 3.40, 0.85, 0.00, 4.35, False, 750, -1, 1600, 3.0, 40, 1386, 0.00509),
    149: _make('SH9', 4.50, 2.45, 0.00, 1.55, 7.00, True, 750, 1800, 1500, 4.4, 40, 1378, 0.00505),
    161: _make('TU1', 0.20, 0.90, 1.50, 0.20, 0.90, True, 2000, 1800, 1600, 0.6, 20, 1606, 0.00885),
    162: _make('TU2', 0.95, 1.80, 1.25, 0.00, 0.20, False, 2000, -1, 1600, 1.0, 30, 1767, 0.00603),
    163: _make('TU3', 1.10, 0.15, 0.25, 0.65, 1.10, True, 1800, 1600, 1400, 1.3, 30, 1611, 0.00359),
    164: _make('TU4', 4.50, 0.00, 0.00, 0.00, 2.00, False, 2300, -1, 2000, 0.5, 12, 2216,  0.01865),
    165: _make('TU5', 4.00, 4.00, 3.00, 0.00, 3.00, False, 1500, -1, 750, 1.0, 25, 1224,  0.02009),
    181: _make('TL1', 1.00, 2.20, 3.60, 0.00, 0.00, False, 2000, -1, -1, 0.2, 30, 1716, 0.04878),
    182: _make('TL2', 1.40, 2.30, 2.20, 0.00, 0.00, False, 2000, -1, -1, 0.2, 25, 1806, 0.04232),
    183: _make('TL3', 0.50, 2.20, 2.80, 0.00, 0.00, False, 2000, -1, -1, 0.3, 20, 1532, 0.02630),
    184: _make('TL4', 0.50, 1.50, 4.20, 0.00, 0.00, False, 2000, -1, -1, 0.4, 25, 1568, 0.02224),
    185: _make('TL5', 1.15, 2.50, 4.40, 0.00, 0.00, False, 2000, -1, 1600, 0.6, 25, 1713, 0.01925),
    186: _make('TL6', 2.40, 1.20, 1.20, 0.00, 0.00, False, 2000, -1, -1, 0.3, 25, 1936, 0.02296),
    187: _make('TL7', 0.30, 1.40, 8.10, 0.00, 0.00, False, 2000, -1, -1, 0.4, 25, 1229, 0.03515),
    188: _make('TL8', 5.80, 1.40, 1.10, 0.00, 0.00, False, 1800, -1, -1, 0.3, 35, 1770,  0.03969),
    189: _make('TL9', 6.65, 3.30, 4.15, 0.00, 0.00, False, 1800, -1, 1600, 0.6, 35, 1733, 0.03372),
    201: _make('SB1', 1.50, 3.00, 11.00, 0.00, 0.00, False, 2000, -1, -1, 1.0, 25, 1653, 0.02224),
    202: _make('SB2', 4.50, 4.25, 4.00, 0.00, 0.00, False, 2000, -1, -1, 1.0, 25, 1884,  0.01829),
    203: _make('SB3', 5.50, 2.75, 3.00, 0.00, 0.00, False, 2000, -1, -1, 1.2, 25, 1935, 0.01345),
    204: _make('SB4', 5.25, 3.50, 5.25, 0.00, 0.00, False, 2000, -1, -1, 2.7, 25, 1907, 0.00744)
}