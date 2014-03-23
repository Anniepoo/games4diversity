$( function() {
	
			var vworldupdatepengine = new Pengine({
                oncreate: handleCreate,
                onsuccess: handleSuccess,
				 onerror: handleError 
            });
            function handleCreate () {
				if(false) {
					alert("created");
					vworldupdatepengine.ask("pen_get_vworld(X)", {
						template:'X'                
					});
				}
            }
            function handleSuccess() {
				if(false) {
					alert("success");
					alert(this.data);
				}
            }
			 function handleError() {
				 if(false) {
				    alert("error");
					alert("error is, Im afraid, " + this.data);
			   }
            }
			
});