// collapse sidebar into buttons
$(function() {
  var $el2 = $(".skin-blue");
  $el2.addClass("sidebar-mini");
  
  
  var $sidebarInput = $("#maps");
  var $logo = $(".logo");
  $(".sidebar-toggle").click(function() {
    $sidebarInput.toggle(400);
    $logo.toggle(400);
  });
});