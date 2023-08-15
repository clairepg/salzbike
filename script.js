  Shiny.addCustomMessageHandler('resetDrawnShape', function(data) {
      Shiny.onInputChange('drawn_shape', null);
    });