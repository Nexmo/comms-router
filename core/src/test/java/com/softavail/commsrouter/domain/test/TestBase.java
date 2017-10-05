/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.softavail.commsrouter.domain.test;

import com.softavail.commsrouter.domain.Router;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import org.junit.After;
import org.junit.Before;

/**
 * @author G.Ivanov
 */
public class TestBase {

    protected static EntityManagerFactory emf;
    protected static EntityManager em;
    protected Router testRouter;

    //Connects to the in-memory h2 database
    @Before
    public void initDb() {
        emf = Persistence.createEntityManagerFactory("mnf-pu-test");
        em = emf.createEntityManager();
        createTable();
        createRouter("name_one", "description_one", "01");
    }

    @After
    public void closeDb() {
        em.close();
        emf.close();
    }

    //Creates the table for the router objects
    public void createTable() {
        em.getTransaction().begin();
        em.createNativeQuery("CREATE TABLE router (name varchar(255),description varchar(255),id varchar(255),version number)").executeUpdate();
        em.getTransaction().commit();
    }

    //Creates and adds a new router object to the DB
    public void createRouter(String name, String description, String id) {
        em.getTransaction().begin();
        Router r = new Router();
        r.setDescription(description);
        r.setName(name);
        r.setId(id);
        r.setVersion(1);
        em.persist(r);
        em.getTransaction().commit();
    }
}
