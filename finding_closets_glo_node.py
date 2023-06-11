#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 30 10:56:01 2021

@author: stanislav
"""

#%%

import pandas as pd
import xarray as xr
import geopandas
from scipy import spatial

from os import chdir

# =============================================================================
# Нам нужно выбрать ближайшие узлы из сетки glo_001_030 к рабочей сетке на 178
# узлов. Из-за высокого разрешения сетки glo_001_030 (0.083° × 0.083°) и 
# крупных масштабов исследуемых процессов можно не использовать интерполяцию.
# Т.к. сетка регулярная и не изменяется из года в год, а размер файлов 
# glo_001_030 достаточно большой, то выберем один год, на основе которого 
# составим общий массив наиболее близких узлов к рабочей сетке nodes_178.
# Этот массив будет использоваться как маска выбора для все последующих лет. 
# =============================================================================

#%%
chdir('/home/stanislav/RProjects/subsurface_layer_2021')

glo_year_df = xr.open_dataset('data/GLOBAL_REANALYSIS_PHY_001_030/glo_1994.nc')
nodes_178 = geopandas.read_file('data/nodes_178.csv')
nodes_178.geometry = geopandas.points_from_xy(nodes_178.lon, 
                                              nodes_178.lat)
nodes_178 = nodes_178.set_crs('EPSG:4326')
nodes_178 = nodes_178.to_crs('EPSG:3576')

nodes_178['x'], nodes_178['y'] =  nodes_178.geometry.x, nodes_178.geometry.y   

#%%
gp_glo_year_df = glo_year_df.isel(depth=0,
                                  latitude=range(360))\
                            .sel(latitude=nodes_178.lat, 
                                 longitude=nodes_178.lon,
                                 method='nearest')\
                            .mean('time')\
                            .to_dataframe()\
                            .dropna(axis=0)\
                            .reset_index()
                         
gp_glo_year_df = gp_glo_year_df.groupby(["latitude", "longitude"]).\
                 mean().reset_index()
#%%               
gp_glo_year_df = geopandas.GeoDataFrame(gp_glo_year_df, 
                               geometry=geopandas.points_from_xy(gp_glo_year_df.longitude,
                                                                 gp_glo_year_df.latitude),
                               crs='EPSG:4326')
gp_glo_year_df = gp_glo_year_df.to_crs('EPSG:3576')
 
gp_glo_year_df['x'], gp_glo_year_df['y'] = gp_glo_year_df.geometry.x, gp_glo_year_df.geometry.y               

#%%

tree = spatial.KDTree(list(zip(nodes_178.x,
                               nodes_178.y)))

a = tree.query_ball_point(list(zip(gp_glo_year_df.x,
                                   gp_glo_year_df.y)),
                          r=10000)

gp_glo_year_df['a'] = a

sat_node = 0

glo_node_dict = {"sat_node":[],
                "lon_glo":[],
                "lat_glo":[],
                "lon_n":[],
                "lat_n":[],
                "node_n":[]
                }

for index, row in gp_glo_year_df.iterrows():
    if row.a:
        sat_node+=1
        for point in row.a:
            glo_node_dict["lon_glo"].append(row.longitude)
            glo_node_dict["lat_glo"].append(row.latitude)
            glo_node_dict["lon_n"].append(nodes_178.iloc[point].lon)
            glo_node_dict["lat_n"].append(nodes_178.iloc[point].lat)
            glo_node_dict["node_n"].append(int(nodes_178.iloc[point].node))
            glo_node_dict['sat_node'].append(f"{sat_node}")

df = pd.DataFrame(glo_node_dict)    

df.to_csv('data/glo_node_list_to_merge.csv')       
