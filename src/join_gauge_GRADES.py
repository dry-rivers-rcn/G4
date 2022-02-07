# Assessing placement bias of the global river gauge network
# Nature Sustainability
# Authors: Corey A. Krabbenhoft, George H. Allen, Peirong Lin, Sarah E. Godsey, Daniel C. Allen, Ryan M. Burrows, Amanda G. DelVecchia, Ken M. Fritz, Margaret Shanafield
# Amy J. Burgin, Margaret Zimmer, Thibault Datry, Walter K. Dodds, C. Nathan Jones, Meryl C. Mims, Catherin Franklin, John C. Hammond, Samuel C. Zipper, Adam S. Ward, 
# Katie H. Costigan, Hylke E. Beck, and Julian D. Olden

# Date: 2/7/2022

# This code all gauge locations, and spatially joins them with GRADES river segments
# output is the joined table of gauge ID (stationid) with GRADES river ID (COMID)

#required library
import geopandas as gpd
import pandas as pd
from shapely.geometry import Point

def find_nearest_river(dfpp,dfll,buffersize):
    '''
    This function finds the nearest river reach ID for each gauge
    input: dfpp: point shapefile of the gauges; dfll: line shapefile of GRADES
    '''
    #create buffer
    print('   create buffer... wait ...')
    poly = dfpp.buffer(buffersize)
    polygpd = gpd.GeoDataFrame(dfpp[['stationid', 'lon', 'lat']],geometry=poly)

    #spatial join
    print('   spatial join with flowlines.. wait ...')
    join = gpd.sjoin(polygpd,dfll,how='inner',op='intersects')
    merge=join.merge(dfll,on='COMID',how='left')
    print('   calculating distance.. wait ...')
    merge['distance']=[Point(merge['lon'][i],merge['lat'][i]).distance(merge['geometry_y'][i]) for i in range(0,len(merge))]
    join11 = merge.groupby(['stationid']).agg({'distance':'min'}).reset_index() #min dist: width and MERIT
    merge11 = join11.merge(merge,on=['stationid','distance'],how='left')
    final = merge11[['stationid','COMID','distance','lon','lat']]
    
    return final


if __name__ == '__main__':

    #read latlon of all gauges (this is a combined gauge location database of GSIM and Beck at al)
    df = pd.read_csv('New_gauge_list_revisions.csv')[['stationid','lat','lon','source']]
    points = [Point(df.lon[j],df.lat[j]) for j in range(len(df))]
    
    #create GeoDataFrame
    dfpp = gpd.GeoDataFrame(df,geometry=points)
    
    #read GRADES river segments and perform spatial join
    buffersize = 0.05 #~5km
    allpoints = []
    for pfaf in range(1,9):
    	#GRADES river segment downloadable from http://hydrology.princeton.edu/data/mpan/MERIT_Basins/MERIT_Hydro_v07_Basins_v01/pfaf_level_01/
        fin = '~/XXX/riv_pfaf_%01d_MERIT_Hydro_v07_Basins_v01.shp'%pfaf
        print('... intersecting with %s ...'%fin)
        dfll = gpd.read_file(fin)
        allpoints.append(find_nearest_river(dfpp,dfll,buffersize))
    allpoints = pd.concat(allpoints)

    #save to file
    fon = 'stationid_GRADES_v07_join.csv'
    print('... writing to %s ...'%fon)
    allpoints.to_csv(fon,index=False)
                        
                         
