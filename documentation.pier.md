

##Introduction

Have you ever wanted to develop advanced client side applications without having to deal with the asynchronous 
nature of Javascript? Wouldn't you love to share your models between your client and your server? Then Tide
is made for you\. Tide is a client\-side web framework developed in Pharo and Amber\. [Tide](https://github.com/tide-framework/tide) offers seamless communications between [Amber](http://amber-lang.net) and  [Pharo](http://pharo-project.org)\.


An application developed in Tide is composed of two parts: one part in Pharo to define what we call a presenter model and one part in Amber for the client\-side widgets\. Tide ensures the communication between these two parts\. Tide exchanges information using the `JSON` format\. The `JSON` is built from  Pharo objects and sent through the network to Amber\. Having both data and actions sent to Amber makes Tide a very good communication protocol between the client and server\.

But there is something fundamentally different in Tide: The `JSON` contains data exposed from objects in Pharo, but it also contains callback information to perform actions from Amber to Pharo objects\. Let us look at the implications raised\. Generating JSON is trivial and used more and more as a exchange format, replacing XML\. However, having operations \(callbacks\) describing how to interact with data is a key difference: It lets you factor out the logic of your application\. You do not get just data and are forced to 
recreate on the client\-side some logic\. You get fully described data, therefore it is easier to build generic and reusable client\-side widgets manipulating data\. 

This documentation will teach how to install and use Tide through examples\.  
We will also learn its architecture and more advanced topics\.

SD: What we will learn concretely\.




## Installing Tide



###1\.  Prerequisites

Tide requires several libraries\. It of course depends on Pharo and Amber\. Amber
itself requires `nodejs` and `bower` to install its own dependencies\. The 
Pharo\-side of Tide requires Zinc, which is part of the default image since 
Pharo 2\.0\. Tide however has only been tested with Pharo 3\.0\.



####1\.1\.  NodeJs

Go to [nodejs\.org](http://nodejs.org) and install `node` \(and `npm`\) for your
platform\.



####1\.2\.  Bower

Bower is a package manager for the web, and Amber uses Bower to manage 
dependencies\. The simplest way to use bower is to install it globally as 
follows:




    $ npm install -g bower





####1\.3\.  Pharo

Tide requires Pharo 3\.0\. The simplest way to install it is to evaluate the 
following:

&nbsp;


    $ curl get.pharo.org/30+vm | bash



To start the Pharo image, evaluate:

&nbsp;


    $ ./pharo-ui Pharo.image





#####1\.3\.1\.  Preparing the Pharo image

SD: We can also do it from the command line and it would be good to show how\. 
SD:Would be good to have one release of tide 
to avoid development\. 

Once you get the Pharo window open, you have to install the Tide backend part\. 
This means bringing the Pharo code you cloned from GitHub?? into the Pharo image\.

&nbsp;

-  Click on the background of the Pharo window
-  In the World menu that appears, click on `Workspace`
-  In that window, evaluate: \(you type the thing, select the text and then right 
  click and select "Do It" from the menu\)\.

&nbsp;


    Metacello new
      configuration: 'Tide';
      version: #development;
      repository: 'http://www.smalltalkhub.com/mc/Pharo/MetaRepoForPharo30/main';
      load.






SD: Why the following is not in the troubleshooting session and bower install is not executed before?

When this is finished, evaluate:

&nbsp;


    TDDispatcher tideIndexPageUrl inspect



When first executed, you will get an error saying you must execute bower 
install in a particular directory\. Open a terminal, change to the right 
directory, and execute:

&nbsp;


    $ bower install



SD: add screenshots

Back in the Pharo window, close the error message and evaluate the same instruction 
again:

&nbsp;


    TDDispatcher tideIndexPageUrl inspect



This should give you the URL at which your web browser should be pointed to\. 
Now copy this URL, open your web browser and paste it in the browser's address bar\.

SD: why do we need to do this inspects? because it gives the impression that everything is manual?




####1\.4\.  Starting the server

The `TDServer` class provides a convenient way to start/stop a Tide server, using
Zinc behind the scenes:

&nbsp;


    TDServer startOn: 5000. "Start the server on port 5000"
    TDServer stop. "Stop any running server"













## A first example: the traditional counter

To get started with Tide, we will implement the traditional counter example as shown in Figure [1\.1](#tideCounter)\. 
Note that Tide already includes such an example in the `Tide-Examples` package that you can refer to\.
But better follow step by step the example\.


<a name="tideCounter"></a><figure><img src="images/tide-counter.png" width="60%"></img><figcaption>A tide counter</figcaption></figure>

SD: picture should be cut in horizontal

Developing an application in Tide consists in two main elements, the presenter part developed in Pharo subclassing some Tide classes
 and the client side developed in Amber\. 

A counter application should contain two buttons, one to increase and the other one to decrease a count\. It should also display the count value to the user\. While this application might seem extremely simplistic, it already shows some of the 
core principles behind Tide: Presenters and Proxies\.




###2\.  The Pharo presenter part
We start by creating the `MyCounter` class in Pharo by subclassing `TDPResenter`\.

&nbsp;


    TDPresenter subclass: #MyCounter
    	instanceVariableNames: 'count'
    	classVariableNames: ''
    	category: 'MyCounter'



Note that not all "exposed" objects have to be subclasses of `TDPresenter`\. As we will
see later, any object can be exposed to Amber using a `TDModelPresenter` instance
on the domain object\. SD: we should also say that latter we will explain what is a TDPresenter\.

Our class has one instance variable `count` that we initialize to `0`:

&nbsp;


    MyCounter >> initialize
        super initialize.
        count := 0



To display the count value to the user, we will need to expose `count` using an accessor\.
We also add two methods to increase and decrease our counter:

&nbsp;


    MyCounter >> count
        ^ count
    
    MyCounter >> increase
        count := count + 1
    
    MyCounter >> decrease
        count := count - 1



The final step we need to add the our counter is pragmas\. Pragmas are 
method metadata\. Tide uses pragmas to expose data \(called state in Tide\) 
and callbacks \(called actions\) to Amber\. Here's our final version of the 
counter class:

&nbsp;


    MyCounter >> count
        <state>
        ^ count
    
    MyCounter >> increase
        <action>
        count := count + 1
    
    MyCounter >> increase
        <action>
        count := count - 1





###3\.  Registering applications with handlers
We now have to create an entry point with our counter presenter in the Tide server\.
To register the entry point, evaluate:

&nbsp;


    MyCounter registerAt: 'my-counter'.




If we perform a request at `http://localhost:5000/my-counter`, we get the following 
`JSON` data back:

&nbsp;


    {
      "__id__":"bwv8m74bhgzmv0dgvzptuy4py",
      "actions":{
        "increase":"/my-counter?_callback=359446426",
        "decrease":"/my-counter?_callback=523483752"
      },
      "state":{
        "count":0
      }
    }



SD: how can I for example get an inspector on the MyCounter instance to increase it\. So that the developer sees it live\.





###4\.  Stepping back

We can learn a couple of points from the preceding example:

&nbsp;

-  Presenter classes are registered as handlers, not instances\. Tide will create "per session" instances of the registered class meaning that presenters are not shared between user sessions\.
-  The entry point will have a `handler` associated with a fixed entry point  url `'/my-counter'`\. When someone will query that registered url, the presenter will generate `JSON` data corresponding to its state and actions, and the handler to send it back in a response to the request\.
-  Sending JSON is common and trivial using a Pharo package such as NeoJSON\. What is much more interesting with Tide is the fact that exchanged data is described with the operations that can be applied to it\. It provides an object\-oriented view on the data\. You get serialized active objects and not plain dead data\.




###5\.  The Amber part of the application

The next step in our example is to create the Amber\-side of this counter application\.

SD: say that Amber will do it for us: 
We will use Amber to render an HTML view of our counter, and perform actions using proxies
back to the counter defined in Pharo\.



####5\.1\.  The client\-side API

On the client\-side, root presenters exposed as handler can be accessed by creating proxies:
SD: It is not clear that we have presenters on Pharo and Amber\. We should explain that before section presenters\.

&nbsp;


    myProxy := TDClientProxy on: '/my-counter'.



Interacting with proxies is performed via messages\. However we have two kinds of messages\. Messages sent to proxies will be resolved using their **state** and **actions** as defined on the server\-side\. SD: what is an actionplus? we did not introduce the terms like that\.

&nbsp;

-  Calls to state methods are resolved locally and synchronously, because the state is passed over to Amber as we previously say in the JSON data\.

&nbsp;

-  Calls to action methods perform requests that will result in performing the corresponding
method on the Pharo object asynchronously\. Once the action is performed, the proxy will
be automatically updated with possible new state and actions\.





####5\.2\.  Handling asynchronous calls

Since action calls are not synchronous, Tide proxies have a special method `then:` used
to perform actions only when and if the action is resolved and the proxy updated\.


Sending a message that activates a state method is synchronous as shown in the following snippet\.
&nbsp;


    "synchronous state call"
    myProxy count. "=> 0"



Now sending a message that activates a callback is asynchronous and as such we should use the `then:` message when we want to access the state as shown below: 
&nbsp;


    "async action call"
    myProxy increase; then: [
        myProxy count "=> 1" ]



SD: the developer in Pharo could use a convention to make this explicit\. Is it wise to have countState?



####5\.3\.  The widget class

In Amber's IDE, create a new class `MyCounterWidget`\. 

&nbsp;


    Widget subclass: #MyCounterWidget
    	instanceVariableNames: 'counter header'
    	package: 'Tide-Amber-Examples'



The widget class has two instance variables: `counter`, which holds a proxy over the 
Pharo counter object, and `header` which  holds a reference on the header tag brush to
update the UI\.

To initialize our counter widget, we connect it to the Pharo counter presenter as follows:

&nbsp;


    MyCounterWidget >> initialize
        super initialize.
        counter := TDClientProxy on: '/my-counter'



Note that `'/my-counter'` is the path to the server\-side handler for our counter presenter\.

We can now create the rendering methods as follows

&nbsp;


    MyCounterWidget >> render
        counter connect then: [
            self appendToJQuery: 'body' asJQuery ]
    
    MyCounterWidget >> renderOn: html
    	header := html h1 with: counter count asString.
    	html button 
    		with: '++';
    		onClick: [ self increase ].
    	html button 
    		with: '--';
    		onClick: [ self decrease ]
    
    MyCounterWidget >> update
    	header contents: [ :html |
    		html with: counter count asString ]



The `render` method waits for the counter to be connected, then appends the widget to the
`body` element of the page \(using the `renderOn:` method\)\.

SD: why the previous part is not in the framework\. Why any user will have to define it?
SD: where is the link between appendTOJQuery and renderOn:?

  `renderOn:` is a typical widget rendering method using the builtin Amber `HTMLCanvas`\.
The `count` message send to the `counter` proxy will be resolved as a state accessor as
defined on the server\-side\.

Finally instead of updating the entire HTML contents of the counter, `update` will only 
update the relevant part, the header\.

We still miss two methods to actually increase and decrease our counter:

&nbsp;


    MyCounterWidget >> increase
    	self counter increase.
    	self counter then: [ self update ]
    
    MyCounterWidget >> decrease
    	self counter decrease.
    	self counter then: [ self update ]



SD: we also miss counter\. is it a method that we should define?


Here's a screenshot of the final counter application:

<a name=""></a>![](images/tide-counter.png "file://images/tide-counter.png")







## Actions

We have seen in a nutshell in the previous sections how Tide actions work\. They allow callbacks
to be performed from Amber to Pharo objects\.

What we haven't discussed yet is how action callbacks in Tide can pass arguments to Pharo objects\.



###6\.  Action arguments



####6\.1\.  Literals

Literal objects can be send as arguments to Tide actions\. They will be converted to JSON and back in
Pharo\. Any literal that can be serialized to JSON can be send as an argument:

&nbsp;

-  Numbers
-  Booleans
-  Strings
-  Dictionaries
-  Arrays \(and OrderedCollections\)

As an example, we can improve the counter to be able to increase it by any number instead of one:

&nbsp;


    MyCounter >> increaseBy: anInteger
        <action>
        count := count + anInteger



On the Amber side, we can change the `increase` method to increase the counter by 8:

&nbsp;


    MyCounterWidget >> increase
        self counter increaseBy: 8; then: [
            self update ]





####6\.2\.  References

While sending literals from Amber to Pharo is definitely useful and convenient, it is barely enough for more
complicated scenarios, where more complex objects have to be sent as arguments\.

To manage this use case, Tide allows references to presenters to be used as action arguments too\. 
This means that any presenter proxy in Amber can be used as an argument to an action argument, and 
that identity will be preserved on the Pharo side when the action message will be sent\.



###7\.  Chaining actions

Tide actions can be easily chained without breaking the sequential flow of the application code, 
using promises\. This is an important property of action callbacks, because all requests done in 
the client must be asynchronous, quickly leading to "spaghetti callbacks" code\.

SD: here we should have a JS example of async and show how we can express it in tide\. 




####7\.1\.  Back to the counter example

The following code snippet shows how `increase` calls to our counter are chained\.
SD: using `then:`

&nbsp;


    myCounter := TDClientProxy on: '/my-counter'.
    
    myCounter increase; increase.
    myCounter then: [ myCounter count ]. "=> 2"
    
    10 timesRepeat: [ myCounter decrease ].
    myCounter then: [ myCounter count ]. "=> -8"





## Presenters

Tide makes it easy to create presenters on domain objects\.



###8\.  Root presenters

In any Tide application, some presenters must be always accessible at a fixed url\. They are called 
in Tide terminology root presenters\. Root presenters are the entry points of Tide application\.

We already saw one root presenter, the `MyCounter` class\.

To register a presenter class, use `TDPresenter>>registerAt:`\. Tide will register the presenter 
class with the corresponding path and create instances per session\.

Note that only one instance of registered root presenter class will be created per session\. This 
ensures that all actions will be performed on the same object\.




###9\.  Answering new presenters from action callbacks

One important aspect of Tide presenters is the ability to answer new presenters from action methods\.
From one root presenters several other presenters are accessed by reachability, allowing you to define
the flow of your application from one root presenter\.

Any object answered from an action method that is not a presenter is converted using `#asPresenter`\. 



####9\.1\.  A note about security

&nbsp;<p class="todo">explain how actions answering new presenters are important for security</p>


####9\.2\.  Example: a login presenter

To illustrate the flow and security implied by actions and presenter, we will write a small login 
application\.

Let's start with the login presenter class itself\. In order to minimize unnecessary complexity, the 
`TDLoginPresenter` class will hold a class variable `Users` containing all users

&nbsp;


    TDPresenter subclass: #MyLoginPresenter
    	instanceVariableNames: 'currentUser'
    	classVariableNames: 'Users'
    	category: 'Examples'



&nbsp;


    MyLoginPresenter class >> initialize
        Users := OrederedCollection new
    
    MyLoginPresenter class >> addUser: anUser
        ^ Users add: anUser



To login an user, we validate an username and password against the `Users` collection, and expose 
the `loginUsername:password:` method as a Tide action:

&nbsp;


    MyLoginPresenter >> loginUsername: username password: password
        <action>
        ^ currentUser := Users 
            detect: [ :each | 
                each username = username and: [ each password = password ] ]
            ifNone: [ nil ]



Note that the `#login` method answers the `currentUser` if any\. The user object will be converted
into a presenter \(instance of `TDModelPresenter`\) automatically and sent back to Amber as the 
response of the action call\.

We now only miss an user class to fill the login presenter:

&nbsp;


    Object subclass: #MyUser
    	instanceVariableNames: 'username password'
    	classVariableNames: '
    	category: 'Examples'
    
    MyUser >> username
        <state>
        ^ username
    
    MyUser >> password
        <state>
        ^ password
    
    MyUser >> username: aString
        <action>
        username := aString
    
    MyUser >> password: aString
        <action>
        password := aString



Now we can register our login presenter class and add an user:

&nbsp;


    MyLoginPresenter registerAt: 'my-login'.
    MyLoginPresenter addUser: (MyUser new
        username: 'John';
        password: 'pass';
        yourself)



To try out the login presenter, we can create a login proxy in an Amber workspace and inspect it\.

&nbsp;


    (TDClientProxy on: '/my-login') connect; inspect



Once connected, we can try to login:

&nbsp;


    self 
        loginUsername: 'John' password: 'pass';
        then: [ :user | user inspect ]



Since the login credentials are valid, Tide will create and answer a presenter on our `MyUser`
instance, with all four methods defined as state and action\.

In the user proxy inspector, we can now query and change the password, which would not have been 
possible since the `MyUser` presenter would not have been answered from the `MyLoginPresenter`
instance, thus making the user out of our reach\.

&nbsp;


    self password "=> 'pass'".
    self password: 'another_password'





###10\.  Builtin presenter classes

Tide contains convenient presenter classes builtin\. Builtin presenter classes can be divided into 
two categories:

&nbsp;

-  builtin base presenters \(literal and collection presenters\)
-  model presenters



####10\.1\.  Builtin presenters

Tide provides the following default presenter classes:

&nbsp;

-  `TDCollectionPresenter`, used by default by all collections but `Dictionary`
-  `TDDictionaryPresenter`, the default presenter for dictionaries
-  `TDLiteralPresenter`, the default presenter for booleans, numbers and strings



###11\.  Custom presenters



###12\.  Presenters and security





## Managing sessions



###13\.  Handling session expiration

&nbsp;<p class="todo">talk about the hook when sessions expire</p>


###14\.  TDSessionManager



###15\.  Using custom session classes



## Handlers

&nbsp;<p class="todo">should it be there already? It seems too early to talk about that, but I need to introduce the concept in order to talk about the file upload handler\.</p>



## Managing file uploads

Managing file uploads in the context of a flat\-client application can be cumbersome\. 
The reason is that file uploads with the HTTP protocols were not made for asynchronous 
uploads\. Tide tries to solve this problem by abstracting away the implementation details 
of an AJAX\-friendly file upload with the `TDFileHandler` class\.



###16\.  Creating file upload entry points



## Handling exceptions

&nbsp;<p class="todo">talk about handling exceptions happening in the Pharo\-side from Amber\.</p>


###17\.  TDPresenterExceptionHandler




## A more advanced example: 

We should use the example of the book of Olivier: a comix collection\. 



