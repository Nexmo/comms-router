<template>
  <nav class="level app-levelbar">
    <div class="level-left">
      <div class="level-item">
        <h3 class="subtitle is-5">
          <strong>{{ name }}</strong>
        </h3>
      </div>
    </div>

    <div class="level-right is-hidden-mobile">
      <breadcrumb :list="list"></breadcrumb>
    </div>
  </nav>
</template>

<script>
import Breadcrumb from 'vue-bulma-breadcrumb'

export default {
  components: {
    Breadcrumb
  },

  data () {
    return {
      list: null
    }
  },

  created () {
    this.getList()
  },

  computed: {
    name () {
      return this.$i18n.t(this.$route.name);
    }
  },

  methods: {
    getList () {
      let matched = this.$route.matched.filter(item => item.name)
      let first = matched[0]
      if (first && (first.name !== this.$i18n.t('message.home') || first.path !== '')) {
        matched = [{ name: this.$i18n.t('message.home'), path: '/' }].concat(matched)
      }

      for (let match of matched) {
        match.name = this.$i18n.t(match.name);
      }

      this.list = matched
    }
  },

  watch: {
    $route () {
      this.getList()
    }
  }
}
</script>

<style lang="scss" scoped>

  /*.breadcrumb a.is-active {*/
    /*padding: 0 !important;*/
  /*}*/
</style>
