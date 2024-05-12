// geometry in this script is a shapefile of potential big sagebrush habitat (Schlaefer et al. 2012)
// best to take this one year at a time, update the year as necessary
// landsat 8 will work for 2012-2023, 1985-2012 can be done with landsat 4 - 7
var sat_image2 = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2")
  .filterDate('2013-01-01','2023-09-30')
  .select('SR_B1','SR_B2','SR_B3','SR_B4','SR_B5','SR_B7')
  .filterBounds(geometry)

// NDVI function
var addNDVI = function(image) {
  var ndvi = image.normalizedDifference(['SR_B4', 'SR_B3']).rename('NDVI');
  return image.addBands(ndvi);
};

// add NDVI to the Image Collection

var withNDVI2 = sat_image2.map(addNDVI)


var ls_2023 = ee.ImageCollection(withNDVI2).filterDate('2023-05-01','2023-08-01');



//need to make 2023 one image
var imagery_2023 = ls_2023.reduce(ee.Reducer.median())


var imagery_2023 = imagery_2023.clip(geometry)
var imagery_2023 = imagery_2023.toFloat()


Export.image.toDrive({
  image:imagery_2023,
  description: '2023_allSagebrush_Landsat',
  scale: 30,
  region: geometry,
  fileFormat: 'GeoTIFF',
  maxPixels: 1e12,
  crs:'EPSG:4326',
  formatOptions: {
    cloudOptimized: true
  },
  skipEmptyTiles: true
});

Map.addLayer(imagery_2023)
