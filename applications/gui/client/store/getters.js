const pkg = state => state.pkg;
const app = state => state.app;
const device = state => state.app.device;
const sidebar = state => state.app.sidebar;
const effect = state => state.app.effect;
const menuitems = state => state.menu.items;
const menuitemsGeneral = state => state.menu.itemsGeneral;
const componententry = state => {
  return state.menu.items.filter(c => c.meta && c.meta.label === 'Components')[0]
};
const loggedIn = state => state.app.auth.loggedIn;
const routerRef = state => state.app.routerRef;

export {
  pkg,
  app,
  device,
  sidebar,
  effect,
  menuitems,
  menuitemsGeneral,
  componententry,
  loggedIn,
  routerRef
}
