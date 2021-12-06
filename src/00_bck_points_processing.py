# Modules/libraries
import arcpy
import os

# Overwrite outputs
arcpy.env.overwriteOutput = True

# Define directories
gdb_dir = r"Mammals.gdb"
data_input = r"data output/temp_bck_dir"

# Set workspace
arcpy.env.workspace = data_input

# Copy occurrence points, background points, and buffer to GDB (assume that RSA grid has already been imported to GDB)
arcpy.CopyFeatures_management("bck_points.shp", os.path.join(gdb_dir, "bck_points"))
arcpy.CopyFeatures_management("occ_buffer.shp", os.path.join(gdb_dir, "occ_buffer"))
arcpy.CopyFeatures_management("occ_points.shp", os.path.join(gdb_dir, "occ_points"))

# Set workspace
arcpy.env.workspace = gdb_dir

# Intersect occurrence points and RSA grid
grid_sub = arcpy.SelectLayerByLocation_management(in_layer="RSA_grid", overlap_type="INTERSECT",
                                                  select_features="occ_points", selection_type="NEW_SELECTION")
# Write to GDB
arcpy.CopyFeatures_management(in_features=grid_sub, out_feature_class="grid_select")

# Intersect background points with grid cells that contain occurrence points
bck_intersect = arcpy.SelectLayerByLocation_management(in_layer="bck_points", overlap_type="INTERSECT",
                                                       select_features="grid_select", selection_type="NEW_SELECTION")

# Delete the offending background points
arcpy.DeleteFeatures_management(in_features=bck_intersect)

# Check features
arcpy.ListFeatureClasses()

# Write new set of background points to shapefile
out_dir = r"data output/temp_bck_dir"
arcpy.CopyFeatures_management("bck_points", os.path.join(out_dir, "bck_points_updated"))

print("BACKGROUND POINT PROCESSING COMPLETE")
