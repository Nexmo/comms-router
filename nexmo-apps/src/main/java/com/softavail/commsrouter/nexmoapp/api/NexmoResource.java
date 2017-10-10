package com.softavail.commsrouter.nexmoapp.api;

import com.nexmo.client.voice.CallEvent;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

/**
 * Created by @author mapuo on 09.10.17.
 */
@Path("{applicationId}/nexmo")
@Produces({MediaType.APPLICATION_JSON})
@Consumes({MediaType.APPLICATION_JSON})
public class NexmoResource {

  @GET
  @Path("inbound")
  public Response answerInbound(
      @PathParam("applicationId") String applicationId,
      @QueryParam("from") String from,
      @QueryParam("to") String to,
      @QueryParam("conversation_uuid") String uuid) {

    // TODO

    return Response.ok()
        .build();
  }

  @GET
  @Path("outbound")
  public Response answerOutbound(
      @PathParam("applicationId") String applicationId,
      @QueryParam("conversation_uuid") String uuid) {

    // TODO

    return Response.ok()
        .build();
  }

  @POST
  @Path("event")
  public Response event(
      @PathParam("applicationId") String applicationId,
      CallEvent callEvent) {

    // TODO

    return Response.ok()
        .build();
  }

}
