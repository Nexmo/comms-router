<template>
  <section class="app-main" :style="[hiddenSidebarStyle]">
    <div class="container is-fluid is-marginless app-content">
      <!--<levelbar></levelbar>-->
      <transition
        mode="out-in"
        enter-active-class="fadeIn"
        leave-active-class="fadeOut"
        appear>
        <router-view class="animated"></router-view>
      </transition>
    </div>
  </section>
</template>

<script>
import Levelbar from './Levelbar'
import { mapGetters } from 'vuex'

export default {
  computed: {
    ...mapGetters({
      sidebar: 'sidebar'
    }),
    hiddenSidebarStyle () {
      return this.sidebar.hidden ? { 'margin-left': 0 } : null
    }
  },

  components: {
    Levelbar
  }
}
</script>

<style lang="scss">
@import '~assets/volta/scss/lib/_mediaqueries.scss';

.app-main {
  flex: 1;
  height: 100vh;
  overflow: auto;
  padding-bottom: 20px;
  padding-left: 10px;
  padding-right: 20px;
  padding-top: 15px;
  height: calc(100vh - 70px);

 /* @media #{$M-plus} {
    height: calc(100vh - 120px);
  }*/
}

.app-content {
  padding: 20px;
}
</style>
