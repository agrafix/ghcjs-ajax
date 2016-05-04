/**
 * Send AJAX request
 *
 * @param {string} url Request target url
 * @param {string} method HTTP Method, eg "GET" or "POST"
 * @param {string|null} requestBody Request Body
 * @param {string|null} contentType The content type
 * @param {function(number, string)} onComplete success handler
 */
function ghcjsajax$sendRequest(url, method, requestBody, contentType, onComplete) {
    var req = ghcjsajax$createXMLHTTPObject();
    if (!req) {
        onComplete(500, "Browser not supported");
        return;
    }
    req.open(method, url, true);
    req.setRequestHeader('User-Agent','XMLHTTP/1.0');
    if (requestBody && contentType) {
        req.setRequestHeader('Content-type', contentType);
    }
    req.onreadystatechange = function () {
        if (req.readyState != 4) {
            return;
        }
        if (req.status != 200 && req.status != 304) {
            onComplete(req.status, req.responseText);
            return;
        }
        onComplete(req.status, req.responseText);
    };
    if (req.readyState == 4) {
        onComplete(500, "Bad ready state");
        return;
    }

    if (requestBody) {
        req.send(requestBody);
    } else {
        req.send();
    }
}

var ghcjsajax$XMLHttpFactories = [
    function () { return new XMLHttpRequest(); },
    function () { return new ActiveXObject("Msxml2.XMLHTTP"); },
    function () { return new ActiveXObject("Msxml3.XMLHTTP"); },
    function () { return new ActiveXObject("Microsoft.XMLHTTP"); }
];

function ghcjsajax$createXMLHTTPObject() {
    var xmlhttp = false;
    for (var i=0; i<ghcjsajax$XMLHttpFactories.length; i++) {
        try {
            xmlhttp = ghcjsajax$XMLHttpFactories[i]();
        }
        catch (e) {
            continue;
        }
        break;
    }
    return xmlhttp;
}
