<!--
MIT License

Copyright (c) 2017 Rafael Beraldo

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

-->
<template>
    <div class="Vlt-autocomplete">
        <vlt-input
            :val="newValue"
            v-model="newValue"
            :disabled="disabled"
            v-bind="$attrs"
            @focus="focused"
            @blur="onBlur"
            @keyup.native.esc.prevent="isActive = false"
            @keydown.native.tab="tabPressed"
            @keydown.native.enter.prevent="enterPressed"
            @keydown.native.up.prevent="keyArrows('up')"
            @keydown.native.down.prevent="keyArrows('down')"
        />

        <transition name="fade">
            <div
                class="Vlt-dropdown__panel"
                :class="{ 'is-opened-top': !isListInViewportVertically }"
                v-show="isActive && data.length > 0"
                ref="dropdown">
                <div class="Vlt-dropdown__panel__content">
                    <a
                        v-for="(option, index) in data"
                        :key="index"
                        class="Vlt-dropdown__link"
                        @click="setSelected(option)">
                        <span v-html="getValue(option, true)"/>
                    </a>
                    <div v-if="data.length === 0">
                        No results
                    </div>
                </div>
            </div>
        </transition>
    </div>
</template>

<script>
    import { getValueByPath, escapeRegExpChars } from 'buefy/src/utils/helpers'
    import FormElementMixin from 'buefy/src/utils/FormElementMixin'
    import VltInput from './VltInput'

    export default {
        name: 'vlt-autocomplete',

        components: {
            VltInput
        },

        inheritAttrs: false,
        props: {
            disabled: Boolean,
            value: [Number, String],
            data: {
                type: Array,
                default: () => []
            },
            field: {
                type: String,
                default: 'value'
            },
            keepFirst: Boolean,
            clearOnSelect: Boolean,
            openOnFocus: Boolean
        },
        data() {
            return {
                selected: null,
                hovered: null,
                isActive: false,
                newValue: this.value,
                isListInViewportVertically: true,
                hasFocus: false,
                _isAutocomplete: true,
                _elementRef: 'input'
            }
        },
        watch: {
            /**
             * When dropdown is toggled, check the visibility to know when
             * to open upwards.
             */
            isActive(active) {
                if (active) {
                    this.calcDropdownInViewportVertical()
                } else {
                    this.$nextTick(() => this.setHovered(null))
                    // Timeout to wait for the animation to finish before recalculating
                    setTimeout(() => {
                        this.calcDropdownInViewportVertical()
                    }, 100)
                }
            },

            /**
             * When updating input's value
             *   1. Emit changes
             *   2. If value isn't the same as selected, set null
             *   3. Close dropdown if value is clear or else open it
             */
            newValue(value) {
                this.$emit('input', value)
                // Check if selected is invalid
                const currentValue = this.getValue(this.selected)
                if (currentValue && currentValue !== value) {
                    this.setSelected(null, false)
                }
                // Close dropdown if input is clear or else open it
                if (this.hasFocus && (!this.openOnFocus || value)) {
                    this.isActive = !!value
                }
            },

            /**
             * When v-model is changed:
             *   1. Update internal value.
             *   2. If it's invalid, validate again.
             */
            value(value) {
                this.newValue = value
            },

            /**
             * Select first option if "keep-first
             */
            data(value) {
                // Keep first option always pre-selected
                if (this.keepFirst) {
                    this.selectFirstOption(value)
                }
            }
        },
        methods: {
            /**
             * Set which option is currently hovered.
             */
            setHovered(option) {
                if (option === undefined) return

                this.hovered = option
            },

            /**
             * Set which option is currently selected, update v-model,
             * update input value and close dropdown.
             */
            setSelected(option, closeDropdown = true) {
                if (option === undefined) return

                this.selected = option
                this.$emit('select', this.selected)
                if (this.selected !== null) {
                    this.newValue = this.clearOnSelect ? '' : this.getValue(this.selected)
                }
                closeDropdown && this.$nextTick(() => { this.isActive = false })
            },

            /**
             * Select first option
             */
            selectFirstOption(options) {
                this.$nextTick(() => {
                    if (options.length) {
                        // If has visible data or open on focus, keep updating the hovered
                        if (this.openOnFocus || (this.newValue !== '' && this.hovered !== options[0])) {
                            this.setHovered(options[0])
                        }
                    } else {
                        this.setHovered(null)
                    }
                })
            },

            /**
             * Enter key listener.
             * Select the hovered option.
             */
            enterPressed() {
                if (this.hovered === null) return
                this.setSelected(this.hovered)
            },

            /**
             * Tab key listener.
             * Select hovered option if it exists, close dropdown, then allow
             * native handling to move to next tabbable element.
             */
            tabPressed() {
                if (this.hovered === null) {
                    this.isActive = false
                    return
                }
                this.setSelected(this.hovered)
            },

            /**
             * Close dropdown if clicked outside.
             */
            clickedOutside(event) {
                this.isActive = false
            },

            /**
             * Return display text for the input.
             * If object, get value from path, or else just the value.
             * If hightlight, find the text with regex and make bold.
             */
            getValue(option, isHighlight = false) {
                if (!option) return

                const value = typeof option === 'object'
                    ? getValueByPath(option, this.field)
                    : option

                const escapedValue = typeof this.newValue === 'string'
                    ? escapeRegExpChars(this.newValue)
                    : this.newValue
                const regex = new RegExp(`(${escapedValue})`, 'gi')

                return isHighlight
                    ? value.replace(regex, '<b>$1</b>')
                    : value
            },

            /**
             * Calculate if the dropdown is vertically visible when activated,
             * otherwise it is openened upwards.
             */
            calcDropdownInViewportVertical() {
                this.$nextTick(() => {
                    /**
                     * this.$refs.dropdown may be undefined
                     * when Autocomplete is conditional rendered
                     */
                    if (this.$refs.dropdown === undefined) return

                    const rect = this.$refs.dropdown.getBoundingClientRect()

                    this.isListInViewportVertically = (
                        rect.top >= 0 &&
                        rect.bottom <= (window.innerHeight ||
                            document.documentElement.clientHeight)
                    )
                })
            },

            /**
             * Arrows keys listener.
             * If dropdown is active, set hovered option, or else just open.
             */
            keyArrows(direction) {
                const sum = direction === 'down' ? 1 : -1
                if (this.isActive) {
                    let index = this.data.indexOf(this.hovered) + sum
                    index = index > this.data.length - 1 ? this.data.length : index
                    index = index < 0 ? 0 : index

                    this.setHovered(this.data[index])

                    const list = this.$refs.dropdown.querySelector('.dropdown-content')
                    const element = list.querySelectorAll('.dropdown-item:not(.is-disabled)')[index]

                    if (!element) return

                    const visMin = list.scrollTop
                    const visMax = list.scrollTop + list.clientHeight - element.clientHeight

                    if (element.offsetTop < visMin) {
                        list.scrollTop = element.offsetTop
                    } else if (element.offsetTop >= visMax) {
                        list.scrollTop = (
                            element.offsetTop -
                            list.clientHeight +
                            element.clientHeight
                        )
                    }
                } else {
                    this.isActive = true
                }
            },

            /**
             * Focus listener.
             * If value is the same as selected, select all text.
             */
            focused(event) {
                if (this.getValue(this.selected) === this.newValue) {
                    this.$el.querySelector('input').select()
                }
                if (this.openOnFocus) {
                    this.isActive = true
                    if (this.keepFirst) {
                        this.selectFirstOption(this.data)
                    }
                }
                this.hasFocus = true
                this.$emit('focus', event)
            },

            /**
             * Blur listener.
            */
            onBlur(event) {
                this.hasFocus = false
                this.$emit('blur', event)
            }
        },
        created() {
            if (typeof window !== 'undefined') {
                document.addEventListener('click', this.clickedOutside)
                window.addEventListener('resize', this.calcDropdownInViewportVertical)
            }
        },
        beforeDestroy() {
            if (typeof window !== 'undefined') {
                document.removeEventListener('click', this.clickedOutside)
                window.removeEventListener('resize', this.calcDropdownInViewportVertical)
            }
        }
    }
</script>
<style lang="scss" scoped>
  .Vlt-dropdown__panel {
      max-height: 100vh;
      overflow: visible;
      transition: 0.3s;
    }
</style>
