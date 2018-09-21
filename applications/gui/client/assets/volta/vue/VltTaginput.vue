<!--MIT License

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
    <div class="taginput control" :class="[size, rootClasses]">
        <div
            class="taginput-container"
            :class="[statusType, size, containerClasses]"
            :disabled="disabled"
            @click="hasInput && focus($event)">
            <vlt-badge
                v-for="(tag, index) in tags"
                :key="index"
                small
                dismissable
                stacked
                @dismissed="removeTag(tag)">
                {{ getNormalizedTagText(tag) }}
            </vlt-badge>

            <vlt-autocomplete
                ref="autocomplete"
                v-if="hasInput"
                v-model="newTag"
                v-bind="$attrs"
                :data="data"
                :field="field"
                :icon="icon"
                :icon-pack="iconPack"
                :maxlength="maxlength"
                :has-counter="false"
                :size="size"
                :disabled="disabled"
                :loading="loading"
                keep-first
                @focus="onFocus"
                @blur="customOnBlur"
                @keyup.native="keyup"
                @select="onSelect"/>
        </div>

        <p v-if="maxtags || maxlength" class="help counter">
            <template v-if="maxlength && valueLength > 0">
                {{ valueLength }} / {{ maxlength }}
            </template>
            <template v-else-if="maxtags">
                {{ tagsLength }} / {{ maxtags }}
            </template>
        </p>
    </div>
</template>

<script>
	import _ from 'lodash'
	import VltAutocomplete from './VltAutocomplete'
	import VltBadge from './VltBadge'
    import FormElementMixin from 'buefy/src/utils/FormElementMixin'

    export default {
        name: 'vlt-taginput',
        components: {
        	VltBadge,
            VltAutocomplete
        },
        mixins: [FormElementMixin],
        inheritAttrs: false,
        props: {
            value: {
                type: Array,
                default: () => []
            },
            data: {
                type: Array,
                default: () => []
            },
            type: String,
            rounded: {
                type: Boolean,
                default: false
            },
            attached: {
                type: Boolean,
                default: false
            },
            maxtags: {
                type: [Number, String],
                required: false
            },
            field: {
                type: String,
                default: 'value'
            },
            autocomplete: Boolean,
            disabled: Boolean,
            confirmKeyCodes: {
                type: Array,
                default: () => [13, 188]
            },
            allowNew: Boolean
        },
        data() {
            return {
                tags: this.value || [],
                newTag: '',
                _elementRef: 'input',
                _isTaginput: true
            }
        },
        computed: {
            rootClasses() {
                return {
                    'is-expanded': this.expanded
                }
            },

            containerClasses() {
                return {
                    'is-focused': this.isFocused,
                    'is-focusable': this.hasInput
                }
            },

            valueLength() {
                return this.newTag.trim().length
            },

            /**
             * Show the input field if a maxtags hasn't been set or reached.
             */
            hasInput() {
                return this.maxtags == null || this.tagsLength < this.maxtags
            },

            tagsLength() {
                return this.tags.length
            }
        },
        watch: {
            /**
             * When v-model is changed set internal value.
             */
            value(value) {
                this.tags = value
            },

            newTag(value) {
                this.$emit('typing', value.trim())
            },

            hasInput() {
                if (!this.hasInput) this.onBlur()
            }
        },
        methods: {
            addTag(tag) {
                const tagToAdd = tag || this.newTag.trim()

                // Add the tag input if it is not blank or previously added.
                if (tagToAdd && this.tags.indexOf(tagToAdd) === -1) {
                    this.tags.push(tagToAdd)
                    this.$emit('input', this.tags)
                    this.$emit('add', tagToAdd)
                }

                this.newTag = ''
            },

            getNormalizedTagText(tag) {
                if (typeof tag === 'object') {
					return this.field.split('.').reduce((o, i) => o[i], tag)
                }

                return tag
            },

            customOnBlur($event) {
                this.onBlur($event)

                $event.stopPropagation()

                // Add tag on-blur if not select only
                if (!this.autocomplete) this.addTag()
            },

            onSelect(option) {
                if (!option) return

                this.addTag(option)
                this.$nextTick(() => {
                    this.newTag = ''
                })
            },

            removeTag(tag) {
            	this.tags = _.pull(this.tags, tag)
            },

            removeLastTag() {
                if (this.tagsLength > 0) {
                    this.newTag = this.removeTag(this.tags[this.tagsLength - 1])
                }
            },

            keyup(event) {
                // Stop if is to accept select only
                if (this.autocomplete && !this.allowNew) return

                if (this.confirmKeyCodes.indexOf(event.keyCode) >= 0) {
                	event.stopPropagation();
                    this.addTag()
                }
            }
        }
    }
</script>
