$(function() {
	function updatebyajax() {
	     $.get( "/people", function(data) {

			 var people = data.list;

			 for(var i = 0 ; i < people.length ; i++) {
			    var name = people[i].name;

				$("#" + name ).animate( {
				left: "" + people[i].x + "px",
				top: "" + people[i].y + "px"
				}, 1200);
				$("#" + name ).attr("title", "set" + people[i].tooltip);
			 }
			 setTimeout( updatebyajax, 1000);
		}).fail(function() {
		     console.log("ajax error - cant get people");
		});
	}
	
	setTimeout( updatebyajax, 1000);
});
