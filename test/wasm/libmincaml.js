(function(global, f){
    if ('object' === typeof exports){
        f(exports);
    } else{
        global.LibMinCaml = f({});
    }
})(this, function(exports){
    // Helper function to adapt external writer.
    let writer;
    if ('object' === typeof MinCamlWriter){
        writer = (str)=> MinCamlWriter.write(str);
    }else if ('object' === typeof process){
        writer = (str)=> process.stdout.write(str);
    }else{
        writer = ()=>{};
    }

    // Library
    exports.lib = {
        min_caml_print_int(v){
            writer(String(v));
        },
    };
});
