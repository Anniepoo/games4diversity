$( function() {
	
			var vworldupdatepengine = new Pengine({
                oncreate: handleCreate,
                onsuccess: handleSuccess,
				 onerror: handleSuccess 
            });
            function handleCreate () {
                vworldupdatepengine.ask("pen_get_vworld(X)", {
                    template:'X'
                });
            }
            function handleSuccess() {
                console.log(this.data);
            }
			
});