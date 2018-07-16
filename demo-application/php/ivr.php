<?php
use Psr\Http\Message\ServerRequestInterface;
use React\EventLoop\Factory;
use React\Http\Response;
use React\Http\Server;
// sample ivr using nexmo api
// 
require 'vendor/autoload.php';

$config=["base_path" => "http://e08f9800.ngrok.io/ivr"];

$loop = React\EventLoop\Factory::create();

function curl($url, $method, $body){
    echo "\n";
    echo $url;
    echo $body;
    $ch = curl_init();
    curl_setopt($ch, CURLOPT_URL, "http://localhost:8080/comms-router-web/api/routers/" . $url);
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
    curl_setopt($ch, CURLOPT_CUSTOMREQUEST, $method);
    if ($body !=""){
        curl_setopt($ch, CURLOPT_POSTFIELDS, $body);
    }
    curl_setopt($ch, CURLOPT_HTTPHEADER, array( 'Content-type: application/json'));
    $output = curl_exec($ch);
    curl_close($ch);
    echo "\n" . $output;
    return json_decode($output,true);
}

function createTask($router, $user_context, $requirements){
    return curl($router . "/tasks", "POST", "{\"tag\":null,\"callbackUrl\":\"http://localhost:4343/ivr?nexmo=create\",\"userContext\":" . $user_context . ",\"requirements\":" . json_encode($requirements) . ",\"queueRef\":null,\"planRef\":\"simple-menu\"}");
};

function getTask($router, $task_id){
    return curl($router . "/tasks/" . $task_id, "GET","");
};

function completeTask($router, $task){
    return curl($router . "/tasks/" . $task,"POST",'{"state":"completed"}');
};

function getAgent($router, $agent_id){
    return curl( $router . "/agents/" . $agent_id, "GET", "");
};

// handlers:
$handleNexmoEvent = function(ServerRequestInterface $request) {
    $body = $request->getBody()->__toString();
    $info=json_decode($body,true);
    echo "\n   Event:\n";
    echo "   " . $body;
    if($info['status']=='completed'){
        // find task associated to this conversation id and complete it.
        echo "\n   completing task";
        completeTask("router-ivr", getTask("router-ivr","?q=userContext.conversation=attr=" . $info["conversation_uuid"])[0]["ref"]);
    }
    echo "\n";
    return new Response(200);
};

$handleNexmoAnswer = function(ServerRequestInterface $request) use ($config){
    return new Response(200
                        ,array( 'Content-Type' => 'text/plain' )
                        , json_encode ([[
                            'action' => 'talk',
                            'bargeIn' => true,
                            'text' => 'Thanks for calling our service. Press 1 for english support, Press 2 for Espanian support, Press 3 for English sales, press 4 for Espanian sales. Complete with #.'],[
                            'action' => 'input',
                            'eventUrl' => [$config['base_path'] . '?nexmo=ivr'],
                            'timeOut' => '20',
                            'submitOnHash' => true
                        ]]));
};

function selectRequirements($num){
   switch ($num) {
   case "1" : return [ "language" => "en", "department" => "support"];
   case "2" :return [ "language" => "es", "department" => "support"];
   case "3" :return [ "language" => "en", "department" => "sales"];
   case "4" :return [ "language" => "es", "department" => "sales"];
   default: return [ "unknow" => "unknown dtmf code"];
   };
}

$handleNexmoIVR = function(ServerRequestInterface $request) use($handleNexmoAnswer, $config){
    $body = $request->getBody()->__toString();
    echo "\n from nexmo\n";
    //echo $body;
    $info=json_decode($body,true);
    var_dump($info["dtmf"]);
    $task = createTask("router-ivr",
               '{"conversation":"' . $info["conversation_uuid"] . '","callid":"someinfo"}',
               selectRequirements($info["dtmf"]));
    
    return new Response(200
                        ,array( 'Content-Type' => 'text/plain' )
                        , json_encode (
                            [[
                                'action' => 'talk',
                                'text' => 'Please wait agent to appear'
                            ],[
                                'action' => 'input',
                                'eventUrl' => [$config['base_path'] . '?nexmo=wait&task=' . $task["ref"]]
                            ]]));
};

$handleWait = function(ServerRequestInterface $request) use($config){
    $body = $request->getBody()->__toString();
    $task_id=$request->getQueryParams()["task"];
    echo "\n wait for task" . $task_id . "\n";
    $taskInfo=getTask("router-ivr",$task_id);
    
    switch ($taskInfo["state"]) {
    case "assigned":
        echo "\n Start call";
        $agent = getAgent("router-ivr",$taskInfo["agentRef"]);
        return new Response(200
                            , array( 'Content-Type' => 'text/plain' )
                            , json_encode (
                                [
                                    [
                                        'action' => 'talk',
                                        'text' => 'Connecting to agent with phone ' . $agent["address"]
                                    ],

                                    [
                                        'action' => 'connect',
                                        'endpoint' => [[
                                            "type" => "phone",
                                            "number"=> $agent["address"]
                                        ]],
                                        'limit' => 5
                                    ],
                                    [
                                        'action' => 'talk',
                                        'text' => 'Thank you'
                                    ]
                                ]));
    default:
        return new Response(200
                            , array( 'Content-Type' => 'text/plain' )
                            , json_encode (
                                [[
                                    'action' => 'talk',
                                    'text' => 'Task is in state ' . $taskInfo["state"] . '. Please wait agent to appear.'
                                ],
                                 [
                                     'action' => 'input',
                                     'eventUrl' => [$config['base_path'] . '?nexmo=wait&task=' . $task_id]
                                 ]
                                ]));

    };
};

$handleTaskStarted= function(ServerRequestInterface $request) {
    $body = $request->getBody()->__toString();
    $json_start = json_decode($body, true);
    echo "\ntask started";
    $router = $json_start["task"]["routerRef"];
    $task = $json_start["task"]["ref"];
    return new Response(200);
};

$socket = new React\Socket\Server(4343, $loop);
(new Server(
    function (ServerRequestInterface $request) 
        use($handleTaskStarted, $handleNexmoEvent, $handleNexmoAnswer, $handleNexmoIVR, $handleWait){
            $queryParams = $request->getQueryParams();
            // dispatch based on query parameter 'nexmo'
            return [ "create" => $handleTaskStarted,
                     "ivr" => $handleNexmoIVR,
                     "wait" => $handleWait,
                     "event" => $handleNexmoEvent,
                     "answer" => $handleNexmoAnswer][$queryParams["nexmo"]]($request);
        }))->listen($socket);

$loop->run();
