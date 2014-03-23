$(function() {
	function updatebyajax() {
	     $.get( "/people", function(data) {
		     console.log(data);
			 setTimeout( updatebyajax, 0.5);
		}).fail(function() {
		     console.log("ajax error - cant get people");
		});
	}
	
	setTimeout( updatebyajax, 0.5);
});
