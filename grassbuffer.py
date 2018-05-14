import sys
import arcpy
import os
arcpy.env.workspace = "C:\Users\langzx\Documents\ArcGIS\Default.gdb"
arcpy.env.overwriteOutput = True
path = "C:\Users\langzx\Documents"
outFile = open(r"C:\Users\langzx\Documents\testGrass.txt", "a")
outFile.write("ID, Percent Wetland, Percent grass, Percent water")
a = 1
for i in range (1,5):
##  nothing
 if not(os.path.exists(path + "\sest" + str(i) + ".txt")):
  print ("Running number " + str(i))
  arcpy.Select_analysis("C:\Users\langzx\Desktop\WRS2017\grassbuffer\WCMOBuffers.shp", "C:\Users\langzx\Desktop\WRS2017\MOs\WCMO.shp", '"FID" = '+str(i+1))
 # get the WRS id
  c=""
  fcSearchCL = arcpy.da.SearchCursor("C:\Users\langzx\Desktop\WRS2017\MOs\WCMO.shp", ["Site_ID"])
  for fc1 in fcSearchCL:
   c = str(fc1[0])
  print (c)
  #clipping the grid
  arcpy.Clip_management("C:\Users\langzx\Desktop\WRS2017\grassbuffer\CDL_2015_clip_20160429095147_1008827168.tif", "#", "res", "C:\Users\langzx\Desktop\WRS2017\MOs\WCMO.shp", "#", "ClippingGeometry", "MAINTAIN_EXTENT")
  #calculating the number of cells, the area of the grass, and water
  fcSearch = arcpy.da.SearchCursor("C:\Users\langzx\Documents\ArcGIS\Default.gdb\\res", ["VALUE", "Count"])
  grass=0;
  wetland =0;
  total=0;
  water = 0;
  for fcRow in fcSearch:
   total=total+fcRow[1];
   if fcRow[0] in[ 37, 61, 60, 62, 152, 171, 176, 181, 195]:
   #if fcRow[0] in[242, 58, 250, 176, 57, 37, 59, 224, 27,61, 60, 195]:
    grass = grass+fcRow[1]
   if fcRow[0] in[87, 195]:
    wetland = wetland+fcRow[1]
   if fcRow[0] in[111, 83]:
    water = water + fcRow[1]
  # calculating the percentages
  pwater = water/total;
  pag=grass/total
  pwlnd =  wetland/total;
  outFile.write(c + "," + str(pwlnd) + ", " + str(pag) + "," + str(pwater)+ "\n")
  temp = open(r"C:\Users\langzx\Documents\Test_"+str(i)+".txt", "w")
  temp.close()
  del fcSearchCL, fcRow
outFile.close()
