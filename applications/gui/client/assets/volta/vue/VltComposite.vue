<template>
  	<div class="Vlt-composite">
		<div class="Vlt-composite__prepend Vlt-composite__prepend--icon">
			<!-- <svg><use xlink:href="icon"/></svg> -->
		</div>
		<div class="Vlt-input">
			<input :type="type" :id="id" />
		</div>
	</div>
</template>

<script>
    export default {
      name: "vlt-composite",

      props: {
      	icon: String,
        id: String,
        type: String
      },

      data() {
        return {
          valid: null,
        }
      }
    }
</script>