<template>
  <div>
    <h1>Login</h1>
   
    <div v-if="error" class="Vlt-callout Vlt-callout--critical">
      <i></i>
      <div class="Vlt-callout__content">
        {{ error }}
      </div>
    </div>

    <form v-on:submit.prevent="login">
      <vlt-field big label="Email">
        <vlt-input v-model="data.body.username" :val="data.body.username" placeholder="email@example.org" label="Email"/>
      </vlt-field>
      <vlt-field big>
        <vlt-input type="password" placeholder="Password" v-model="data.body.password" label="Password"/>
      </vlt-field>

      <vlt-field>
        <vlt-checkbox label="Remember me" :val="data.rememberMe" v-model="data.rememberMe"/>
      </vlt-field>

      <div class="Vlt-grid Vlt-grid--narrow">
        <div class="Vlt-col Vlt-col--center">
          <button type="submit" class="Vlt-btn Vlt-btn--large Vlt-btn--block Vlt-btn--app Vlt-btn--primary">Login</button>
        </div>
      </div>  
    </form>
  </div>
</template>

<script>
  import { VltCheckbox, VltField, VltInput } from '../../assets/volta/vue'; 

  export default {
    components: {
      VltCheckbox,
      VltField,
      VltInput
    },

    data () {
      return {
        data: {
          body: {
            username: null,
            password: null
          },
          rememberMe: false
        },
        error: null
      }
    },
    mounted () {
      if (this.$auth.redirect()) {
        console.log('Redirect from: ' + this.$auth.redirect().from.name)
      }
      // Can set query parameter here for auth redirect or just do it silently in login redirect.
    },
    methods: {
      login () {
        var redirect = this.$auth.redirect()
        this.$auth.login({
          headers: {
            'Content-Type': 'application/json'
          },
          data: this.data.body,
          rememberMe: this.data.rememberMe,
          redirect: {name: redirect ? redirect.from.name : 'Home'},
          success (res) {
            console.log('Auth Success')
            // console.log('Token: ' + this.$auth.token())
            // console.log(res)
          },
          error (err) {
            if (err.response) {
              // The request was made, but the server responded with a status code
              // that falls out of the range of 2xx
              // console.log(err.response.status)
              // console.log(err.response.data)
              // console.log(err.response.headers)
              this.error = err.response.data
            } else {
              // Something happened in setting up the request that triggered an Error
              console.log('Error', err.message)
            }
            console.log(err.config)
          }
        })
      }
    }
    // filters: {
    //   json: function (value) {
    //     console.log(value)
    //     return value
    //   }
    // }

  }
</script>

<style lang="scss" scoped>
.is-title {
    text-transform: capitalize;
}
</style>
