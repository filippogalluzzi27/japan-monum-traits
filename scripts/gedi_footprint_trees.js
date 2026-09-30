var pts = ee.FeatureCollection('projects/groundwater-500910/assets/jap_trees_gee');

// controllo visivo dei punti sulla mappa
Map.addLayer(pts, {color: 'red'}, 'alberi');
Map.centerObject(pts, 6);

var gedi = ee.ImageCollection('LARSE/GEDI/GEDI02_A_002_MONTHLY')
  .map(function(img) {
    return img.select('rh98')
      .updateMask(img.select('quality_flag').eq(1))
      .updateMask(img.select('degrade_flag').eq(0));
  });

var nObs = gedi.count().unmask(0);

var res = nObs.reduceRegions({
  collection: pts.map(function(f) { return f.buffer(20); }),
  reducer: ee.Reducer.max(),
  scale: 25
});

// esportazione su Google Drive
Export.table.toDrive({
  collection: res.select(['id', 'max'], null, false),
  description: 'gedi_vicinanza_alberi',
  fileFormat: 'CSV'
});