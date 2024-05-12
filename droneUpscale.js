var img1 = ee.Image('users/sww28/CO_01_01_trained');
var img1A = ee.Image('users/sww28/CO_1_Ortho');
var img1Mask = img1A.updateMask(img1A.select('b4').gt(0));
var shp1 = img1Mask.select('b4').reduceToVectors({bestEffort:true});
var img1Clip = img1.clip(shp1);
var img1Updated = img1Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img1.projection();
var img1Updated = img1Updated.reproject({crs: projection});
////////////////////////////////////////////
var img2 = ee.Image('users/sww28/CO_01_02_trained');
var img2A = ee.Image('users/sww28/CO_2_Ortho');
var img2Mask = img2A.updateMask(img2A.select('b4').gt(0));
var shp2 = img2Mask.select('b4').reduceToVectors({bestEffort:true});
var img2Clip = img2.clip(shp2);
var img2Updated = img2Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img2.projection();
var img2Updated = img2Updated.reproject({crs: projection});
////////////////////////////////////////////
var img3 = ee.Image('users/sww28/ID_01_01_trained');
var img3A = ee.Image('users/sww28/ID_1_1_Ortho');
var img3Mask = img3A.updateMask(img3A.select('b4').gt(0));
var shp3 = img3Mask.select('b4').reduceToVectors({bestEffort:true});
var img3Clip = img3.clip(shp3);
var img3Updated = img3Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img3.projection();
var img3Updated = img3Updated.reproject({crs: projection});
////////////////////////////////////////////
var img4 = ee.Image('users/sww28/ID_01_02_trained');
var img4A = ee.Image('users/sww28/ID_1_2_Ortho');
var img4Mask = img4A.updateMask(img4A.select('b4').gt(0));
var shp4 = img4Mask.select('b4').reduceToVectors({bestEffort:true});
var img4Clip = img4.clip(shp4);
var img4Updated = img4Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img4.projection();
var img4Updated = img4Updated.reproject({crs: projection});
////////////////////////////////////////////
var img5 = ee.Image('users/sww28/NV_01_01_trained');
var img5A = ee.Image('users/sww28/NV_1_1_Ortho');
var img5Mask = img5A.updateMask(img5A.select('b4').gt(0));
var shp5 = img5Mask.select('b4').reduceToVectors({bestEffort:true});
var img5Clip = img5.clip(shp5);
var img5Updated = img5Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img5.projection();
var img5Updated = img5Updated.reproject({crs: projection});
////////////////////////////////////////////
var img6 = ee.Image('users/sww28/NV_01_02_trained');
var img6A = ee.Image('users/sww28/NV_1_2_Ortho');
var img6Mask = img6A.updateMask(img6A.select('b4').gt(0));
var shp6 = img6Mask.select('b4').reduceToVectors({bestEffort:true});
var img6Clip = img6.clip(shp6);
var img6Updated = img6Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img6.projection();
var img6Updated = img6Updated.reproject({crs: projection});
////////////////////////////////////////////
var img7 = ee.Image('users/sww28/NV_01_03_trained');
var img7A = ee.Image('users/sww28/NV_1_3_Ortho');
var img7Mask = img7A.updateMask(img7A.select('b4').gt(0));
var shp7 = img7Mask.select('b4').reduceToVectors({bestEffort:true});
var img7Clip = img7.clip(shp7);
var img7Updated = img7Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img7.projection();
var img7Updated = img7Updated.reproject({crs: projection});
////////////////////////////////////////////
var img8 = ee.Image('users/sww28/NV_01_04_trained');
var img8A = ee.Image('users/sww28/NV_1_4_Ortho');
var img8Mask = img8A.updateMask(img8A.select('b4').gt(0));
var shp8 = img8Mask.select('b4').reduceToVectors({bestEffort:true});
var img8Clip = img8.clip(shp8);
var img8Updated = img8Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img8.projection();
var img8Updated = img8Updated.reproject({crs: projection});
////////////////////////////////////////////

var img9 = ee.Image('users/sww28/NV_02_01_trained');
var img9A = ee.Image('users/sww28/NV_2_1_Ortho');
var img9Mask = img9A.updateMask(img9A.select('b4').gt(0));
var shp9 = img9Mask.select('b4').reduceToVectors({bestEffort:true});
var img9Clip = img9.clip(shp9);
var img9Updated = img9Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img9.projection();
var img9Updated = img9Updated.reproject({crs: projection});
////////////////////////////////////////////
var img10 = ee.Image('users/sww28/NV_02_02_trained2');
var img10A = ee.Image('users/sww28/NV_2_2_Ortho');
var img10Mask = img10A.updateMask(img10A.select('b4').gt(0));
var shp10 = img10Mask.select('b4').reduceToVectors({bestEffort:true});
var img10Clip = img10.clip(shp10);
var img10Updated = img10Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img10.projection();
var img10Updated = img10Updated.reproject({crs: projection});
////////////////////////////////////////////
var img11 = ee.Image('users/sww28/NV_02_03_trained');
var img11A = ee.Image('users/sww28/NV_2_3_Ortho');
var img11Mask = img11A.updateMask(img11A.select('b4').gt(0));
var shp11 = img11Mask.select('b4').reduceToVectors({bestEffort:true});
var img11Clip = img11.clip(shp11);
var img11Updated = img11Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img11.projection();
var img11Updated = img11Updated.reproject({crs: projection});
////////////////////////////////////////////
var img12 = ee.Image('users/sww28/UT_01_01_trained');
var img12A = ee.Image('users/sww28/UT_1_1_Ortho');
var img12Mask = img12A.updateMask(img12A.select('b4').gt(0));
var shp12 = img12Mask.select('b4').reduceToVectors({bestEffort:true});
var img12Clip = img12.clip(shp12);
var img12Updated = img12Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img12.projection();
var img12Updated = img12Updated.reproject({crs: projection});
////////////////////////////////////////////
var img13 = ee.Image('users/sww28/UT_01_02_trained');
var img13A = ee.Image('users/sww28/UT_1_2_Ortho');
var img13Mask = img13A.updateMask(img13A.select('b4').gt(0));
var shp13 = img13Mask.select('b4').reduceToVectors({bestEffort:true});
var img13Clip = img13.clip(shp13);
var img13Updated = img13Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img13.projection();
var img13Updated = img13Updated.reproject({crs: projection});
////////////////////////////////////////////
var img14 = ee.Image('users/sww28/UT_01_03_trained');
var img14A = ee.Image('users/sww28/UT_1_3_Ortho');
var img14Mask = img14A.updateMask(img14A.select('b4').gt(0));
var shp14 = img14Mask.select('b4').reduceToVectors({bestEffort:true});
var img14Clip = img14.clip(shp14);
var img14Updated = img14Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img14.projection();
var img14Updated = img14Updated.reproject({crs: projection});
////////////////////////////////////////////
var img15 = ee.Image('users/sww28/UT_02_01_trained');
var img15A = ee.Image('users/sww28/UT_2_1_Ortho');
var img15Mask = img15A.updateMask(img15A.select('b4').gt(0));
var shp15 = img15Mask.select('b4').reduceToVectors({bestEffort:true});
var img15Clip = img15.clip(shp15);
var img15Updated = img15Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img15.projection();
var img15Updated = img15Updated.reproject({crs: projection});
////////////////////////////////////////////
var img16 = ee.Image('users/sww28/UT_02_02_trained');
var img16A = ee.Image('users/sww28/UT_2_2_Ortho');
var img16Mask = img16A.updateMask(img16A.select('b4').gt(0));
var shp16 = img16Mask.select('b4').reduceToVectors({bestEffort:true});
var img16Clip = img16.clip(shp16);
var img16Updated = img16Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img16.projection();
var img16Updated = img16Updated.reproject({crs: projection});
////////////////////////////////////////////
var img17 = ee.Image('users/sww28/UT_02_03_trained');
var img17A = ee.Image('users/sww28/UT_2_3_Ortho');
var img17Mask = img17A.updateMask(img17A.select('b4').gt(0));
var shp17 = img17Mask.select('b4').reduceToVectors({bestEffort:true});
var img17Clip = img17.clip(shp17);
var img17Updated = img17Clip.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});
var projection = img17.projection();
var img17Updated = img17Updated.reproject({crs: projection});
////////////////////////////////////////////
///////////////////////////////////////////////////////////////
var boundary = ee.Geometry.Polygon(
        [[[-125.19975440768887, 49.68839311008749],
          [-125.19975440768887, 33.53151338856081],
          [-103.13920753268887, 33.53151338856081],
          [-103.13920753268887, 49.68839311008749]]], null, false);

var sat_image2 = ee.ImageCollection("LANDSAT/LT05/C02/T1_L2")
  .filterDate('1985-01-01','2004-09-30')
  .select('SR_B1','SR_B2','SR_B3','SR_B4','SR_B5','SR_B7')
  .filterMetadata('CLOUD_COVER', 'less_than', 20)
  //.filterMetadata('IMAGE_QUALITY', 'greater_than',8)
var ls_2023 = ee.ImageCollection(sat_image2).filterDate('1985-05-01','1985-08-01');
//var clip_ls_2014 = ls_2014.map(clp(points))






// NDVI function
var addNDVI = function(image) {
  var ndvi = image.normalizedDifference(['SR_B4', 'SR_B3']).rename('NDVI');
  return image.addBands(ndvi);
};

// add NDVI to the Image Collection

//var ls_2023 = ls_2023.map(addNDVI)

//need to make 2023 one image

var imagery_2023 = ls_2023.reduce(ee.Reducer.median())

//now we need to remove all non sage steppe imagery
//var imagery_2023 = imagery_2023.clip(boundary)



////////////////////////////////////////
var drones = ee.ImageCollection.fromImages([img1Clip,img2Clip,
img3Clip,img4Clip,img5Clip,img6Clip,img7Clip,img8Clip,img8Clip,
img9Clip,img10Clip,img11Clip,img12Clip,img13Clip,img14Clip,
img15Clip,img16Clip,img17Clip])

var dronesMosaic = drones.mosaic()

//var replacement0 = ee.Image(0);
//var replacement1 = ee.Image(1)

//var conditional = function(image) {
  //var image2 = image.where(image.gt(0), replacement0)
//  return image.where(image.eq(0), replacement1);
//};

//var dronesUpdated = conditional(dronesMosaic)
var dronesUpdated = dronesMosaic.remap({
  from: [0,2,3],
  to: [1,0,0],
  defaultValue: 0,
  bandName: 'b1'
});


var projection = img1.projection()
var dronesUpdated = dronesUpdated.reproject({crs: projection})
//Map.addLayer(dronesUpdated)

//var blank = ee.Image('users/sww28/BLANK');

// Display the EVI image near La Honda, California.
//Map.setCenter(-122.3616, 37.5331, 12);
//Map.addLayer(blank);

// Get information about the MODIS projection.
//var lsProjection = sat_image2.first().projection();
var lsProjection = imagery_2023.projection()
print(lsProjection);

// Load and display forest cover data at 30 meters resolution.
//var forest = ee.Image('UMD/hansen/global_forest_change_2015')
  //  .select('treecover2000');
//Map.addLayer(forest, {max: 80}, 'forest cover 30 m');

// Get the forest cover data at MODIS scale and projection.
//var droneMean = dronesUpdated
    // Force the next reprojection to aggregate instead of resampling.
  //  .reduceResolution({
  //    reducer: ee.Reducer.mean(),
  //    bestEffort: true
  //  })
    // Request the data at the scale and projection of the MODIS image.
  //  .reproject({
  //    crs: lsProjection,//.crs(),
  //    scale:30
  //  });

// Aggregated data.
var img1Mean = img1Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var img2Mean = img2Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var img3Mean = img3Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var img4Mean = img4Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var img5Mean = img5Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var img6Mean = img6Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var img7Mean = img7Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var img8Mean = img8Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var img9Mean = img9Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var img10Mean = img10Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var img11Mean = img11Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var img12Mean = img12Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var img13Mean = img13Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var img14Mean = img14Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var img15Mean = img15Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var img16Mean = img16Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var img17Mean = img17Updated
    // Force the next reprojection to aggregate instead of resampling.
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      bestEffort: true
    })
    // Request the data at the scale and projection of the MODIS image.
    .reproject({
      crs: lsProjection,//.crs(),
      scale:30
    });
////////////////////////////////
var drones = ee.ImageCollection.fromImages([img1Mean,img2Mean,
img3Mean,img4Mean,img5Mean,img6Mean,img7Mean,img8Mean,img8Mean,
img9Mean,img10Mean,img11Mean,img12Mean,img13Mean,img14Mean,
img15Mean,img16Mean,img17Mean])

var dronesMean = drones.mosaic()

var img = ee.Image('users/sww28/sageshrb_sgca')
var img = img.reproject({crs:lsProjection})
var feat = img.reduceToVectors({'scale':100,'bestEffort':true})
var feat2 = ee.FeatureCollection('users/sww28/bigsage_extent')
var comb = feat2.merge(feat)
var comb2 = comb.union({'maxError':100})
var full = ee.Feature(comb2.first())

var imagery_2023 = imagery_2023.clip(full)
//var imagery_2023 = imagery_2023.toFloat()
//var imagery_2023 = imagery_2023.reproject({crs:lsProjection})
Export.image.toDrive({
  image:imagery_2023,
  region:boundary,
  description: 'trainingReady',
  scale: 30,
  fileFormat: 'GeoTIFF',
  maxPixels:1e13,
  //crs:lsProjection.crs(),
  //crsTransform:[30,0,341085,0,-30,8808015],
  //shardSize: 100,
  formatOptions: {
    cloudOptimized: true
  },
  skipEmptyTiles: true
});
Map.addLayer(imagery_2023)
