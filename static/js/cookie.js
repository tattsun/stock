var cookie = new Object();
cookie.Get = function(key) {
    var rawCookies = document.cookie;
    var rawKvs = rawCookies.split("; ");
    var kvs = rawKvs.map(function(kv) { return kv.split("="); });
    for(var i = 0;  i < kvs.length; i++) {
        if (kvs[i][0] === key) {
            return decodeURIComponent(kvs[i][1]);
        }
    }
    return null;
};
cookie.Set = function(key, value) {
    document.cookie = key + '=' + encodeURIComponent(value);
};
