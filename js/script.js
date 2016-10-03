

$(document).ready(function() {

    $("#province").click(function(){
        var  val = $(":selected").attr('value');
        if (val != 1)  {
          $("#hospital_div").hide();
        } else {
          $("#hospital_div").show();
        }
    });

  $( "#icuCode" ).focusout(function() {
      Val = $.isNumeric($("#icuCode").val());

      if(Val) {
        $.ajax({
            type: "GET",
            url: "/",
            data: $("#icuCode").val(),
            cache: false,
            success: function(data) {
              console.log(data);
           },
            dataType: "json"
          });
        }
      console.log(Val);
  });

  });






//   $("#newICU").click(function(event){
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
//           url: "",
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
//  event.preventDefault();
//    });
