// Cette fonction assure qu'au moins un critère est sélectionné


js   <- "
$(document).ready(function(){
  $('#somevalue').on('show.bs.select', function(){
    $('a[role=option]').on('click', function(e){
      var selections = $('#somevalue').val();
      if(selections.length === 1 && $(this).hasClass('selected')){
        e.stopImmediatePropagation();
      };
    });
  }).on('hide.bs.select', function(){
    $('a[role=option]').off('click');
  });
});"
