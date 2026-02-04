# ## Daily Update of Nebraska Temperatures from the NWS Live Atlas Feed.

# #### 1. Connecting to the GIS System
import os
from arcgis.gis import GIS

# Establish connection to the ArcGIS Online platform using credentials.
username = os.getenv('AGOL_USERNAME')
password = os.getenv('AGOL_PASSWORD')
gis = GIS("https://www.arcgis.com", username, password)

# #### 2. Accessing Data Sources
# Access the NWS 3-Day Min/Max Temperature Forecast layer from ESRI's Living Atlas.
source_layer = gis.content.get("0ae7cf18df0a4b4d9e7eea665f00500d").layers[1]

# Access the Nebraska boundary layer to extract relevant data.
boundary = gis.content.get("c0c8f91b03bb4df785d2ff813ccb0518")

# Access the destination layer for storing updated Nebraska temperature data.
Feature_service = gis.content.get("5f14713d0d7a40e1b9a80a17afad8458").layers[0]

# #### 3. Processing the Data
from arcgis import features

# Intersect the NWS temperature layer with the Nebraska boundary and overwrite the existing data.
features.manage_data.overlay_layers(
    source_layer, boundary, overlay_type='Intersect',
    output_name=Feature_service, context={"overwrite": True}
)

from datetime import datetime

# Print the timestamp of the latest update to ensure transparency.
print("The temperature map was updated latest on (UTC): ", datetime.now().strftime("%Y-%m-%d %H:%M:%S"))

# #### 4. Updating the Interactive Map
from arcgis.mapping import WebMap
from arcgis.features import FeatureLayer

# Access your WebMap and locate the Nebraska temperature layer.
webmap_item = gis.content.get("402a4c90557042e0bcb9ab210bf0dcc3")
webmap = WebMap(webmap_item)

layer_name = "Nebraska_Daily_Temp_Max"  # The layer to be updated in the WebMap.
layer = None

# Identify the specific layer within the WebMap by name.
for lyr in webmap.layers:
    if lyr.title == layer_name:
        layer = lyr
        break

if layer is None:
    raise ValueError(f"Layer with name '{layer_name}' not found in the WebMap.")

# Access the FeatureLayer for further customization.
feature_layer = FeatureLayer(layer.url)

# Step 1: Clear the current renderer to reset the layer's visual style.
clear_renderer = {
    "type": "simple",
    "symbol": {
        "type": "esriSFS",
        "style": "esriSFSSolid",
        "color": [0, 0, 0, 0],  # Fully transparent fill
        "outline": {
            "color": [0, 0, 0, 0],  # Transparent outline
            "width": 0
        }
    }
}

# Apply the clear renderer to remove old visual styles.
feature_layer.manager.update_definition({"drawingInfo": {"renderer": clear_renderer}})

print("The temperature map was updated latest on (UTC): ", datetime.now().strftime("%Y-%m-%d %H:%M:%S"))

# #### 5. Visual Enhancements
# Calculate the new temperature range based on the latest data.
temperature_field = "Temp"  # Replace with your temperature field name.

# Get minimum and maximum temperature values for the updated data.
stats = feature_layer.query(
    where="1=1",
    out_statistics=[
        {"statisticType": "min", "onStatisticField": temperature_field, "outStatisticFieldName": "min_value"},
        {"statisticType": "max", "onStatisticField": temperature_field, "outStatisticFieldName": "max_value"}
    ]
)

min_value = stats.features[0].attributes['min_value']
max_value = stats.features[0].attributes['max_value']

# Define a color-coded renderer for the temperature field using a gradient style.
new_renderer = {
    "type": "classBreaks",
    "field": temperature_field,
    "classificationMethod": "equalInterval",
    "minValue": min_value,
    "classBreakInfos": [
        {
            "classMinValue": min_value,
            "classMaxValue": max_value,
            "label": f"{min_value} - {max_value}",
            "symbol": {
                "type": "esriSFS",
                "style": "esriSFSSolid",
                "color": None  # Let the visualVariables control the color
            }
        }
    ],
    "visualVariables": [
        {
            "type": "colorInfo",
            "field": temperature_field,
            "stops": [
                {"value": min_value, "color": "#440154"},
                {"value": min_value + (max_value - min_value) * 0.25, "color": "#3b528b"},
                {"value": min_value + (max_value - min_value) * 0.5, "color": "#21918c"},
                {"value": min_value + (max_value - min_value) * 0.75, "color": "#5ec962"},
                {"value": max_value, "color": "#fde725"}
            ],
            "legendOptions": {
                "title": f"{temperature_field} (°F)"
            }
        }
    ]
}

# Apply the new renderer to visually enhance the map layer.
feature_layer.manager.update_definition({"drawingInfo": {"renderer": new_renderer}})
webmap.update()
print(f"Layer style updated using the 'Counts and Amounts (Color)' with 'Point Cloud 1' symbol style.")

# #### 6. Publishing the Updates
# Print the final timestamp to confirm the successful completion of the update process.
print("The temperature map was updated latest on (UTC): ", datetime.now().strftime("%Y-%m-%d %H:%M:%S"))
