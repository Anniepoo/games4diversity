$( function() {
	
			var vworldupdatepengine = new Pengine({
                oncreate: handleCreate,
                onsuccess: handleSuccess,
				 onerror: handleError 
            });
            function handleCreate () {
				alert("created");
				vworldupdatepengine.ask("pen_get_vworld(X)", {
					template:'X'                
				});
            }
            function handleSuccess() {
				alert("success");
				alert(this.data);
            }
			 function handleError() {
				alert("error is, Im afraid, " + this.data);
            }
			
});