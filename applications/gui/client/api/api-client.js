/* eslint-disable */
import axios from 'axios'

const BASE_URL = '/comms-router-web/api';
let mAuthRequiredHandler = undefined;

function setAuthRequiredHandler(handler) {
  mAuthRequiredHandler = handler;
}

// Routers API
function getRouters() {
  const url = `${BASE_URL}/routers`;

  return new Promise(function (resolve, reject) {
    doGet(url, {}, true)
    .then((response) => {
      let tags = getETag(response.headers);
      if (tags) {
        // Split on semicolon
        tags = tags.split(';');
        if (response.data.length === tags.length) {
          response.data =
            response.data.map((e, idx) => addETagMeta(e, tags[idx]));
        }
      }
      resolve({
        data: response.data,
        nextToken: response.headers['x-next-token']
      });
    })
    .catch(status => {
      reject(status);
    });
  });
}

// Agents API
function getAgent(routerRef, agentRef = null) {
  const url = getResourceUrl('agents', routerRef, agentRef);
  return getResource(url, routerRef);
}

function getAgents(routerRef, token = '', perPage = '', filter = '', sort = '') {
  return getList('agents', routerRef, token, perPage, filter, sort);
}

function createAgent(routerRef, createReq) {
  const url = getResourceUrl('agents', routerRef);
  const options = {
    headers: {'Content-Type': 'application/json'}
  };
  return doPost(url, createReq, options);
}

function updateAgent(agent, updateAgent) {
  const url = getResourceUrl('agents', agent);
  const options = addIfMatch({
    headers: {
      'Content-Type': 'application/json'
    }
  }, agent);
  return doPost(url, updateAgent, options);
}

function deleteAgent(routerRef, agentRef) {
  const url = getResourceUrl('agents', routerRef, agentRef);
  return doDelete(url);
}

function setAgentState(routerRef, agent, state) {
  const url = getResourceUrl('agents', routerRef, agent.ref);
  const body = `{"state": "${state}"}`;
  const options = addIfMatch({
    headers: {
      'Content-Type': 'application/json'
    }
  }, agent);

  const prom = new Promise(function (resolve, reject) {
    doPost(url, body, options, true).then((response) => {
      let tag = getETag(response.headers);
      if (tag) {
        setETagMeta(agent, tag);
      }
      resolve(response.data)
    }).catch(error => reject(error));
  });

  return prom;
}

// Queues API
function getQueues(routerRef, token = '', perPage = '', filter = '', sort = '') {
  return getList('queues', routerRef, token, perPage, filter, sort);
}

function getQueue(routerRef, queueRef = null) {
  const url = getResourceUrl('queues', routerRef, queueRef);
  return getResource(url, routerRef);
}

function getQueueTasks(routerRef, queueRef) {
  const url = `${getResourceUrl('queues', routerRef,queueRef)}/tasks`;
  // const url = `${BASE_URL}/routers/${routerRef}/queues/${queueRef}/tasks`;
  return doGet(url);
}

function getQueueSize(routerRef, queueRef) {
  const url = `${getResourceUrl('queues', routerRef,queueRef)}/size`;
  // const url = `${BASE_URL}/routers/${routerRef}/queues/${queueRef}/size`;
  return doGet(url);
}

// Flows (Plans in api) API
function getFlows(routerRef, token = '', perPage = '', filter = '', sort = '') {
  return getList('plans', routerRef, token, perPage, filter, sort);
}

function getFlow(routerRef, planRef) {
  const url = getResourceUrl('plans', routerRef, planRef);
  return doGet(url);
}

function updateFlow(plan, modifiedPlan) {
  const url = getResourceUrl('plans', plan.routerRef, plan.ref);
  const options = addIfMatch({
    headers: {
      'Content-Type': 'application/json'
    }
  }, plan);
  return doPut(url, modifiedPlan, options);
}

function createFlow(routerRef, modifiedPlan) {
  const url = getResourceUrl('plans', routerRef, modifiedPlan.ref);
  const options = {
    headers: {'Content-Type': 'application/json'}
  };
  if (modifiedPlan.ref) {
    delete modifiedPlan.ref;
    return doPut(url, modifiedPlan, options);
  } else {
    delete modifiedPlan.ref;
    return doPost(url, modifiedPlan, options);
  }
}

function deleteFlow(routerRef, planRef) {
  const url = getResourceUrl('plans', routerRef, planRef);
  return doDelete(url);
}

/**
 * Creates a new queue.
 * If queueRef is passed a queue with the specified ID will be created.
 * If the ID already exists the queue will be replaced otherwise new will be created
 * if queueRef is null a queue with server defined ID will be created
 *
 * @param {*} routerRef ID of the router
 * @param {*} queueData json payload for the queue being created
 * @param {*} queueRef user assigned id of the queue
 */
function createQueue(routerRef, queueData, queueRef) {
  const options = {
    headers: {'Content-Type': 'application/json'}
  };

  if (queueRef == null) {
    const url = getResourceUrl('queues', routerRef);
    return doPost(url, queueData, options);
  } else {
    const url = getResourceUrl('queues', routerRef, queueRef);
    return doPut(url, queueData, options);
  }
}

function updateQueue(queue, queueUpdateData) {
  const url = getResourceUrl('queues', queue.routerRef, queue.ref);
  const options = addIfMatch({
    headers: {
      'Content-Type': 'application/json'
    }
  }, queue);
  return doPost(url, queueUpdateData, options);
}

function deleteQueue(routerRef, queueRef) {
  const url = getResourceUrl('queues', routerRef, queueRef);
  return doDelete(url);
}

// Task API
function getTasks(routerRef, offset) {
  const url = `${getResourceUrl('tasks', routerRef)}?page_num=${offset}`;
  // const url = `${BASE_URL}/routers/${routerRef}/tasks?page_num=${offset}`;
  return doGet(url);
}

function findTask(routerRef, offset, predicate) {
  const pageNumber = offset;
  return new Promise(function (resolve, reject) {
    /* missing implementation */
    getTasks(routerRef, pageNumber).then(tasks => {
      if (tasks.filter(predicate).length !== 0) {
        alert("Found:" + JSON.stringify(tasks.filter(predicate)));
        resolve(tasks.filter(predicate)[0]);
      } else {
        resolve(findTask(routerRef, (pageNumber + 1), predicate));
      }
    }).catch(error => {
      console.error(`Search for agents's test failed! ${error}`)
    })
  });
}

function updateTask(routerRef, taskRef, taskData) {
  const url = `${getResourceUrl('tasks', routerRef, taskRef)}`;
  // const url = `${BASE_URL}/routers/${routerRef}/tasks/${taskRef}`;
  const options = {
    headers: {'Content-Type': 'application/json'}
  };
  const body = JSON.stringify(taskData);
  return doPost(url, body, options);
}

function cancelWaitingTask(routerRef, taskRef) {
  //POST "/routers/router-ivr/tasks/TASKID" -H  "accept: application/json" -H  "Content-Type: application/json" -d '{"state":"canceled"}'
  const url = `${getResourceUrl('tasks', routerRef, taskRef)}`;
  // const url = `${BASE_URL}/routers/${routerRef}/tasks/${taskRef}`;

  const options = {
    headers: {'Content-Type': 'application/json'}
  };

  const body = '{"state":"canceled"}';

  return doPost(url, body, options);
}

// Skills API
function getSkills(routerRef, token = '', perPage = '', filter = '', sort = '') {
  return getList('skills', routerRef, token, perPage, filter, sort);
}

function createSkill(routerRef, skill) {
  const url = getResourceUrl('skills', routerRef, skill.ref);
  const options = {
    headers: {'Content-Type': 'application/json'}
  };
  if (skill.ref) {
    delete skill.ref;
    return doPut(url, skill, options);
  } else {
    return doPost(url, skill, options);
  }
}

function updateSkill(skill, modified) {
  if (modified && modified.ref) {
    delete modified.ref;
  }
  const options = addIfMatch({
    headers: {
      'Content-Type': 'application/json'
    }
  }, skill);
  const url = getResourceUrl('skills', skill.routerRef, skill.ref);
  return doPost(url, modified, options);
}

function deleteSkill(routerRef, skillRef) {
  const url = getResourceUrl('skills', routerRef, skillRef);
  return doDelete(url);
}

function getSkill(routerRef, skillRef = null) {
  const url = getResourceUrl('skills', routerRef, skillRef);
  return getResource(url, routerRef);
}

// Add a response interceptor
axios.interceptors.response.use(function (response) {
  return response;
}, function (error) {
  if (error.response && error.response.status === 401 && typeof mAuthRequiredHandler === "function") {
    mAuthRequiredHandler();
  }
  // Do something with response error
  return Promise.reject(error);
});

function getList(resourceName, routerRef, token = '', perPage = '', filter = '', sort = '') {

  const url = getListUrl(resourceName, routerRef, token, perPage, filter, sort);

  return new Promise(function (resolve, reject) {
    doGet(url, {}, true)
    .then((response) => {
      let tags = getETag(response.headers);
      if (tags) {
        // Split on semicolon
        tags = tags.split(';');
        if (response.data.length === tags.length) {
          response.data =
            response.data.map((e, idx) => addETagMeta(e, tags[idx]));
        }
      }
      resolve({
        data: response.data,
        nextToken: response.headers['x-next-token']
      });
    })
    .catch(status => {
      reject(status);
    });
  });
}

function getListUrl(resourceName, routerRef, token = '', perPage = '', filter = '', sort = '') {
  let params = [
    `per_page=${perPage}`,
    `q=${filter}`
  ];
  if (sort) {
    params.push(`sort=${encodeURIComponent(sort)}`)
  }
  if (token) {
    params.push(`token=${token}`);
  }

  const paramsStr = params.join('&');

  return `${getResourceUrl(resourceName, routerRef)}?${paramsStr}`;
  // return `${BASE_URL}/routers/${routerRef}/${resourceName}?${paramsStr}`;
}

function getResourceUrl(resourceName, routerRef, resourceRef = null) {
  let path;
  resourceName = encodeURIComponent(resourceName);

  if (typeof routerRef === 'object'
    && routerRef.hasOwnProperty('routerRef')
    && routerRef.hasOwnProperty('ref')) {
    path = `${encodeURIComponent(routerRef.routerRef)}/${resourceName}/${encodeURIComponent(routerRef.ref)}`;
  } else if (!resourceRef) {
    path = `${encodeURIComponent(routerRef)}/${resourceName}`;
  } else {
    path = `${encodeURIComponent(routerRef)}/${resourceName}/${encodeURIComponent(resourceRef)}`;
  }

  return `${BASE_URL}/routers/${path}`;
}

function getResource(url, resource = null) {
  let options;

  if (resource && resource.__tag__) {
    options = {
      headers: addIfNoneMatch({
        'Content-Type': 'application/json'
      }, resource),
      validateStatus: function (status) {
        return status === 200 || status === 304;
      },
      transformResponse: [function (data, headers) {
        let eTag = getETag(headers);

        if ((!data || data === '') && eTag === resource.__tag__) {
          return resource;
        }

        return data;
      }]
    };
  }

  return doGet(url, options);
}

function getETag(headers) {
  if (headers && headers.etag) {
    let eTag = headers.etag;
    return eTag.substring(eTag.indexOf('"') + 1, eTag.length - 1);
  }
  return null;
}

function addIfMatch(config, resource) {
  if (resource.__tag__) {
    config.headers = Object.assign({
      'If-Match': `"${resource.__tag__}"`
    }, config.headers);
  }
  return config;
}

function addIfNoneMatch(headers, resource) {
  if (resource.__tag__) {
    headers = Object.assign({
      'If-None-Match': `"${resource.__tag__}"`
    }, headers);
  }
  return headers;
}

function setETagMeta(obj, eTag) {
  if (typeof obj.__tag__ === 'undefined') {
    return Object.defineProperty(obj, '__tag__', {value: eTag, writable: true});
  } else {
    obj.__tag__ = eTag;
  }
  return obj;
}

function addETagMeta(obj, eTag) {
  if (typeof obj.__tag__ === 'undefined') {
    return Object.defineProperty(obj, '__tag__', {value: eTag, writable: true});
  }
  return obj;
}

function doGet(url, options, includeHeaders = false) {
  return new Promise(function (resolve, reject) {
    axios.get(url, options)
    .then((response) => {
      if ([200, 304].includes(response.status)) {
        let resource = response.data;
        if (typeof resource === 'string') {
          resource = JSON.parse(response.data);
        }

        if (typeof resource.__tag__ === 'undefined') {
          resource = addETagMeta(resource, getETag(response.headers));
        }

        if (includeHeaders) {
          resolve({data: resource, headers: response.headers});
        } else {
          resolve(resource);
        }
      } else {
        reject(response.status);
      }
    })
    .catch((response) => {
      // console.log("response:", response);
      reject(response.message);
    });
  });

}

function doPost(url, body, options, includeHeaders = false) {
  return new Promise(function (resolve, reject) {
    axios.post(url, body, options)
    .then(function (response) {
      // console.log(response);
      if (!includeHeaders) {
        resolve(response.data);
      } else {
        resolve({data: response.data, headers: response.headers});
      }

    })
    .catch(function (error) {
      // console.log(error);
      reject(error);
    });
  });
}

function doPut(url, body, options) {
  return new Promise(function (resolve, reject) {
    axios.put(url, body, options)
    .then(function (response) {
      // console.log(response);
      resolve(response.data);
    })
    .catch(function (error) {
      // console.log(error);
      reject(error);
    });
  });
}

function doDelete(url) {
  return new Promise(function (resolve, reject) {
    axios.delete(url)
    .then(function (response) {
      // console.log(response);
      resolve(response.data);
    })
    .catch(function (error) {
      // console.log(error);
      reject(error);
    });
  });
}

export {
  /* router functions */
  getRouters,
  /* agent functions */
  getAgent,
  getAgents,
  setAgentState,
  createAgent,
  updateAgent,
  deleteAgent,
  /* queue functions */
  getQueues,
  getQueue,
  getQueueTasks,
  getQueueSize,
  createQueue,
  updateQueue,
  deleteQueue,
  getTasks,
  findTask,
  updateTask,
  cancelWaitingTask,
  /* skills functions */
  getSkills,
  createSkill,
  updateSkill,
  deleteSkill,
  getSkill,
  /* plans functions */
  getFlows,
  getFlow,
  createFlow,
  updateFlow,
  deleteFlow,
  setAuthRequiredHandler,
}
