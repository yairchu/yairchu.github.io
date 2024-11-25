function switchTheme()
{
    var button = document.getElementById("theme-button");
    document.getElementsByTagName("html")[0].classList.toggle("dark");
    button.innerHTML = button.innerHTML == "DAY" ? "NIGHT" : "DAY";
    // Expire in two months
    setCookie("theme", button.innerHTML == "DAY" ? "day" : "night", 60*24*60*60*1000);
    updateGiscusTheme();
}

function updateGiscusTheme()
{
    document.querySelectorAll("iframe").forEach(iframe => {
        iframe.contentWindow.postMessage(
            { giscus: { setConfig: { theme: button.innerHTML == "DAY" ? "light" : "dark" } } },
            "https://giscus.app"
          );
    });
}

let giscusThemeSet = false;
window.addEventListener("message", event => {
    if (event.origin === "https://giscus.app" && !giscusThemeSet) {
        updateGiscusTheme();
        giscusThemeSet = true;
    }
});

function setCookie(cname,cvalue,extime)
{
    var d = new Date();
    d.setTime(d.getTime()+(extime));
    var expires = "expires="+d.toGMTString();
    document.cookie = cname + "=" + cvalue + ";" + expires + ";" + "path=/"; 
}

function getCookie(cname)
{
    var name = cname + "=";
    var ca = document.cookie.split(';');
    for(var i=0; i<ca.length; i++)
    {
        var c = ca[i].trim();
        if (c.indexOf(name)===0) return c.substring(name.length,c.length);
    }
    return "";
}

// Switch theme if cookie is set
if (getCookie('theme')=='night') {
    switchTheme();
}

// Switch theme if button is clicked.
var button = document.getElementById("theme-button");
button.addEventListener('click', switchTheme);
