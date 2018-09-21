const _ = require('lodash');

module.exports = {
	makeNonReactiveCopy(original) {
	  if (! _.isObject(original) || _.isArray(original)) {
	      return original;
	  }

	  let copy = {}
	  let attributes = Object.keys(original)
	  attributes.forEach(attribute => {
	      let attributeValue = Object.getOwnPropertyDescriptor(original, attribute);
	      let value = attributeValue.get ? attributeValue.get() : attributeValue.value;

	      if(_.isObject(value) && !_.isArray(value)) {
	      	value = this.makeNonReactiveCopy(value); 
	      } else if(_.isArray(value)) {
	      	value = _.values(value);
	      }

	      Object.defineProperty(copy, attribute, {
	          __proto__: null,
	          value: value
	      });
	  });

	  return copy;
	},
	backup(original) {
	  return this.makeNonReactiveCopy(original);
	}
}
