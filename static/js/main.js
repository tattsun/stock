var view = new Object();
view.Initialize = function() {
};
view.Login = function() {
    var userid = $("#login-userid").val();
    var password = $("#login-password").val();

    var onLoginFinished = function(isSucceed) {
        if (isSucceed) {
            location.href = "/";
        } else {
            console.log("FAILED");
        }
    };

    var login = new Login(userid, password, onLoginFinished);
    login.Login();
};
view.Logout = function() {
    var login = new Login();
    login.Logout();

    location.href = "/";
};
view.BackToLastpage = function() {
    window.history.back();
};
view.Post = function(id) {
    var isUpdate = true;
    if (typeof id === 'undefined') {
        isUpdate = false;
    }

    var title = $("#post-title").val();
    if (title.length === 0) {
        title = "Untitled";
    }
    var body = $("#post-body").val();

    var ispublic = $("#post-ispublic:checked").val();
    var region = "Private";
    if (ispublic === 'on') {
        region = "Public";
    }

    var tagsRaw = $("#post-tags").val();
    var tagsSplited = tagsRaw.split(",");
    var tags = tagsSplited.map (function(tag) {
        while(tag[0] === ' ') {
            tag = tag.slice(1);
        }
        while(tag[tag.length-1] === ' ') {
            tag = tag.slice(0, tag.length-2);
        }
        return tag;
    });

    var authorizer = new TokenAuthorizer();
    var userid = authorizer.GetUserID();
    var token = authorizer.GetToken();

    var callback = function(json) {
        if (json.statusId === 'Success') {
            location.href = "/" + json.detail;
        } else {
        }
    };

    var url = "/article";
    if (isUpdate) {
        url = "/article/" + id;
    }

    var data = {userid: userid, token: token, region: region, tags: tags.join(","), title: title, body: body};
    console.log(data);
    $.ajax({
        type: "POST",
        url: url,
        dataType: "json",
        data: data,
        success: callback
    });
};
