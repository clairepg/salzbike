 Shiny.addCustomMessageHandler('clearDrawnItems', function(message) {
  // Assuming 'map' is the name of your Leaflet object. Adjust if necessary.
  var drawnItems = window.Shiny.shinyapp.$values.map_drawnItems; 
  if (drawnItems) {
    drawnItems.clearLayers();
  }
});