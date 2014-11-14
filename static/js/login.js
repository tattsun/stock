var Login = function(userid, password, callback) {
    this.userid = userid;
    this.password = password;
    this.callback = callback;

    var _this = this;

    this.Login = function() {
        $.ajax({
            type: "POST",
            url: "/user/login",
            dataType: "json",
            data: {userid: _this.userid, password: _this.password},
            success: function(json) {
                _this.success(json);
            }
        });
    };

    this.Logout = function() {
        cookie.Set("userid", "");
        cookie.Set("token", "");
    };

    this.success = function(json) {
        if (json.statusId === "Token") {
            cookie.Set("userid", _this.userid);
            cookie.Set("token", json.detail);
            _this.callback(true);
        } else {
            _this.callback(false);
        }
    };
};

var TokenAuthorizer = function(callback) {
    this.callback = callback;

    var _this = this;

    this.GetUserID  = function() {
        return cookie.Get("userid");
    };

    this.GetToken = function() {
        return cookie.Get("token");
    };

    this.Authorize = function() {
        var userid = _this.GetUserID();
        var token = _this.GetToken();

        $.ajax({
            type: "POST",
            url: "/user/token",
            dataType: "json",
            data: {userid: userid, token: token},
            success: function(json) {
                _this.success(json);
            }
        });
    };

    this.success = function(json) {
        if (json.statusId === "Success") {
            _this.callback(true);
        } else {
            _this.callback(false);
        }
    };
};
