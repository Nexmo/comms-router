import axios from "axios/index";

const KEY_TOKEN = 'id_token';
const KEY_ACCESS_TOKEN = 'access_token';
const LOGIN_URL = '/comms-router-web/login';
const LOGOUT_URL = '/comms-router-web/logout';

//the login popup
let mPopupWindow = null;
let mToken = null;
//the callback for the auth result
let mAuthHandler;

const receiveMessage = (event) => {
  console.log(`!!!! ${event.data}`)
  if (!mPopupWindow) {
    return;
  }
  if (window.removeEventListener) {
    window.removeEventListener("message", receiveMessage);
  } else {
    window.detachEvent("onmessage", receiveMessage);
  }

  let token = event.data.token;
  if (!token) {
    if (typeof mAuthHandler === 'function') {
      mAuthHandler(true);
    }

    return;
  }
  setIdToken(token);
  let accessToken = event.data.accessToken;
  setAccessToken(accessToken);

  if (typeof mAuthHandler === 'function') {
    mAuthHandler();
  }
};

function setAuthHandler(handler) {
  mAuthHandler = handler;
}

function showLoginForm() {
  if (mPopupWindow && !mPopupWindow.closed) {
    mPopupWindow.focus();
    return;
  }

  const left = (screen.width/2)-(550/2);
  const top = (screen.height/2)-(550/2);
  mPopupWindow = window.open(LOGIN_URL, 'Auth', `toolbar=no, location=no, directories=no, status=no, menubar=no, 
                    scrollbars=yes, resizable=yes, copyhistory=no, width=550, height=550, top=${top}, left=${left}`);
  if (mPopupWindow) {
    if (window.addEventListener) {
      window.addEventListener("message", receiveMessage, false);
    } else {
      window.attachEvent("onmessage", receiveMessage);
    }
    mPopupWindow.focus();
  }
}

function parseJwt (token) {
  //TODO consider using a library: jwt-decode
  try {
    let base64Url = token.split('.')[1];
    let base64 = base64Url.replace(/-/g, '+').replace(/_/g, '/');
    return JSON.parse(window.atob(base64));
  } catch (e) {
    return {};
  }
}

function getIdToken() {
  return localStorage.getItem(KEY_TOKEN);
}

function setIdToken(idToken) {
  localStorage.setItem(KEY_TOKEN, idToken);
}

function getAccessToken() {
  return localStorage.getItem(KEY_ACCESS_TOKEN);
}

function setAccessToken(token) {
  let expDate = getTokenExpDate(getIdToken());
  if (!expDate) {
    return;
  }
  const maxAge = Math.floor((expDate.getTime() - new Date().getTime()) / 1000);
  if (maxAge <= 0) {
    return;
  }
  //TODO temp while backend is fixed
  // setCookie('JSESSIONID', token, maxAge);
  setCookie('JSESSIONID', token);

  localStorage.setItem(KEY_ACCESS_TOKEN, token);
}

function isLoggedIn() {
  const idToken = getIdToken();
  return !!idToken && !isTokenExpired(idToken);
}

function getTokenExpDate(encodedToken) {
  const token = parseJwt(encodedToken);
  if (!token.expiration) { return null; }

  const date = new Date(token.expiration);
  // date.setUTCMilliseconds(token.expiration);

  return date;
}

function isTokenExpired(token) {
  //TODO temp while backend is fixed
  // const expirationDate = getTokenExpDate(token);
  // return expirationDate < new Date();\

  return false;
}

function getUserName() {
  const idToken = getIdToken();
  if (idToken) {
    const token = parseJwt(idToken);
    return token.name;
  }

  return null;
}

//TODO handle the role
function isAdmin() {
  const idToken = getIdToken();
  if (idToken) {
    const token = parseJwt(idToken);
    if (token.roles) {
      for (let role of token.roles) {
        if (role.toUpperCase() === "ADMIN") {
          return true;
        }
      }
    }
  }

  return false;
}

function logout() {
  if (getIdToken()) {
    sendLogout();
  }

  removeCookie('JSESSIONID', '/comms-router-web');
  localStorage.removeItem(KEY_ACCESS_TOKEN);
  localStorage.removeItem(KEY_TOKEN);

  if (mPopupWindow && !mPopupWindow.closed) {
    mPopupWindow.close();
  }
  mPopupWindow = undefined;
}

function sendLogout() {
  axios.get(LOGOUT_URL).then((response) => {
    if (response.status === 200) {
    } else {
    }
  }).catch((response) => {
  });
}

function setCookie (cname, cvalue, maxAge) {
  let expires;

  if (!maxAge) {
    // let d = new Date();
    // d.setTime(d.getTime() + (30*24*60*60*1000));
    // expires = `expires=${d.toGMTString()};`;
    expires = '';
  } else {
    expires = `Max-Age=${maxAge};`;
  }
  let cookie = `${encodeURIComponent(cname)}=${encodeURIComponent(cvalue)}; ${expires} path=/`;
  document.cookie = cookie;
}

function removeCookie (cname, path) {
  document.cookie = encodeURIComponent(cname) + "=; Max-Age=0; path=/";
}

export {
  logout,
  showLoginForm,
  isLoggedIn,
  getUserName,
  isAdmin,
  setAuthHandler
}
