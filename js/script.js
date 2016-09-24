$(document).ready(function(){
    $("#province").click(function(){
        var  val = $(":selected").attr('value');
        if (val != 1)  {
          $("#hospital").hide();
        } else {
          $("#hospital").show();
        }
    });

   $("#newICU").click(function(event){
//
//         var newICU = {
//                 "data": "newICU",
//                 "code": $("#icuCode").val(),
//                 "name": $("#icuName").val(),
//                 "province": $("#province").val(),
//                 "hospital": $("#hospital").val(),
//                 "insurance": $("#insurance").val(),
//                 "icu_type": $("#icu").val()
//         };
//
//         $.ajax({
//           type: "POST",
//           url: "/newICU/" + newICU.code,
//           data: newICU,
//           cache: false,
//           success: function(data) {
//             console.log(data);
// /*            $.get("/newICU/" + newICU.code, function(res) {
//               console.log(res); */
//
//             },
//           dataType: "json"
//         });
  //       event.preventDefault();
    });
});
