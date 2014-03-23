$( function() {
	
	var vworldupdatepengine = new Pengine({
		oncreate: handleCreate,
		onsuccess: handleSuccess,
		 onerror: handleError 
	});
	function handleCreate () {
		vworldupdatepengine.ask("pen_get_vworld(X)", {
			template:'X'                
		});
	}
	function pengine_next () {
		vworldupdatepengine.next();
		setTimeout( pengine_next, 0.5);
	}
	
	function handleSuccess() {
		var unpack = this.data[0];
		for(var i = 0 ; i < unpack.length ; i++) {
			var p = unpack[i].args;

			$("#" + p[0] ).animate( {
				left: "" + p[1] + "px",
				top: "" + p[2] + "px"
				}, 1000);
			$("#" + p[0] ).attr("title", "set" + p[6]);
		}
		
		setTimeout( pengine_next, 0.5);
	}
	function handleError() {
		alert("error is, Im afraid, " + this.data);
	}
});