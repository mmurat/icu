$(document).ready(function(){
    $("#province").click(function(){
        var  val = $(":selected").attr('value');
        if (val != 1)  {
          $("#hospital").hide();
        } else {
          $("#hospital").show();
        }
    });
});
