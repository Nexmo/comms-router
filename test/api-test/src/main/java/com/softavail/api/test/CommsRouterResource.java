package com.softavail.api.test;

public enum CommsRouterResource {
    ROUTER,
    PLAN,
    QUEUE,
    AGENT,
    TASK;

    @Override
    public String toString() {
        return name().toLowerCase();
    }

}
