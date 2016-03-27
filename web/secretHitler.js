sock = new WebSocket("ws://potatobox.no-ip.info:9160")
var allowPlayerClick = false;
var lastShards = []
var $USERNAME$ = ""
var $SERVERNAME$ = ""
var $PLAYERLIST$ = []
var fascistCount = 0
var liberalCount = 0
sock.onopen = function(e){
  document.getElementById("inputbutton").disabled = ''
  chatLog("\nConnected to Master Server")
}
sock.onclose = function(e){
  document.getElementById("inputbutton").disabled = 'disabled'
  showConnected(false)
}
sock.onmessage = function(e){
  console.log(e.data)
  if(e.data.startsWith("Welcome, ")){//Server accepted our username, show connected, hide join
    document.getElementById("inputbutton").innerHTML = "Start Game"
    document.getElementById("inputbutton").onclick = function(){
      sock.send("ready")
      document.getElementById("inputbutton").style.display = "none"
    }
    document.getElementById("inputbox").style.display = "none"
    $USERNAME$ = e.data.split(" ")[1]
  }
  if(e.data.startsWith("Shards: ")){
    if(lastShards == e.data){return;}
    lastShards = e.data
    shardSelect = document.getElementById("shardselect")
    //Get all shard options
    shardList = processShards(e.data)
    shardSelect.options.legnth = 0
    //Add each shard option
    shardList.forEach(function(a){
      var option = document.createElement("option")
      option.text = a[0] + " (" + a[1] + ")"
      option.value = a[0]//Sent to server
      shardSelect.add(option)
    })
    //Add New Shard option
    var newoption = document.createElement("option")
    newoption.text = "New Shard"
    newoption.value = "New"//Sent to server
    shardSelect.add(newoption)
    //Display the shard selector
    document.getElementById("sharddiv").style.display = 'block'
  }
  if(e.data.startsWith("Connected to: ")){
    $SERVERNAME$ = e.data.split(": ")[1]
    showConnected(true)
    document.getElementById("sharddiv").style.display = 'none'
  } 
  if(e.data.startsWith("Chat|")){
    user = e.data.split("|")[1]
    message = e.data.split("|")[2]
    chatLog("\n" + "<" + user + "> " + message)
  }
  if(e.data.startsWith("Join|")){
    newuser = e.data.split("|")[1]
    chatLog("\nSERVER: Player \"" + newuser + "\" has joined!") 
    addPlayer(newuser)
  }
  if(e.data.startsWith("Disconnect|")){
    leftuser = e.data.split("|")[1]
    chatLog("\nSERVER: Player \"" + leftuser + "\" has disconnected!") 
    removePlayer(leftuser)
  }
  if(e.data.startsWith("Ask|")){
    question = e.data.split("|")[1]
    switch (question){
      // "Ask|Vote" -> "Resp|Ja"
      case "Vote":
        //chatLog("\nSERVER: Please cast your vote!")
        showVotebox(true) 
        break
      case "Veto":
        chatLog("\nSERVER: The chancellor has requested a veto! Do you accept?")
        showVotebox(true)
        break
      // "Ask|Chancellor" -> "Resp|asdf"
      case "Chancellor":
        chatLog("\nSERVER: Please select a chancellor!")
        allowPlayerClick = true
        break
      // "Ask|Discard|Policy Fascist,Policy Liberal,Policy Liberal" -> "Resp|Policy Liberal"
      case "Discard":
        chatLog("\nSERVER: Choose a card to discard")
        cards = e.data.split("|")[2].split(",")
        showCardbox(true,cards)
        break
      // "Ask|Play|Policy Fascist,Policy Liberal,Policy Liberal" -> "Resp|Policy Fascist"
      case "Play":
        chatLog("\nSERVER: Choose a card to play")
        cards = e.data.split("|")[2].split(",")
        showCardbox(true,cards)
        //Hijack the cardbox to display our veto button
        if(fascistCount > 4){
          var vetobutton = document.createElement("button")
          vetobutton.innerHTML = "Veto"
          vetobutton.onclick = function(){
            sock.send("Resp|Veto")
            showCardbox(false,[])
          }
          document.getElementById("cardbox").appendChild(vetobutton)
        }
        break
      case "Kill":
        chatLog("\nSERVER: Choose a player to execute")
        allowPlayerClick = true
        break
      case "Investigate":
        chatLog("\nSERVER: Choose a player to investigate")
        allowPlayerClick = true
        break
      case "Kill":
        chatLog("\nSERVER: Choose a player to elect as president")
        allowPlayerClick = true
        break
      default:
        chatLog("\nWARNING: UNKNOWN API CALL REPORT IT. DO IT NOW:\n" + question)
        break
    }
  }
  if(e.data.startsWith("Info|")){
    info = e.data.split("|")[1]
    switch (info){
      case "A Liberal Policy was played!":
        liberalCount += 1
        setCardcount(liberalCount, fascistCount)
        break
      case "A Fascist Policy was played!":
        fascistCount += 1
        setCardcount(liberalCount, fascistCount)
        break
    }
    if(info.startsWith("The Fascists are ")){
      fascists = info.split(" ").slice(3)
      fascists.forEach(function(a){
        addPlayer(a, "fascist" + (Math.floor(Math.random() * 3) + 1) + ".png", true)
      })
    }
    if(info.startsWith("The Liberals are ")){
      liberals = info.split(" ").slice(3)
      liberals.forEach(function(a){
        addPlayer(a, "liberal" + (Math.floor(Math.random() * 4) + 1) + ".png", true)
      })
    }
    if(info.startsWith("You are ")){
      yourrole = info.split(" ")[2]
      switch(yourrole){
        case "liberal":
          addPlayer($USERNAME$,"liberal" + (Math.floor(Math.random() * 4) + 1) + ".png", true)
          break
        case "fascist":
          addPlayer($USERNAME$,"fascist" + (Math.floor(Math.random() * 3) + 1) + ".png", true)
          break
        case "hitler":
          addPlayer($USERNAME$,"hitler.png", true)
          break
      }
    }
    if(info.startsWith("Hitler is ")){
      hitler = info.split(" ")[2]
      addPlayer(hitler,"hitler.png", true)
    }
    if(info.startsWith("The votes were")){
      votes = info.split(": ")[1].split("\n").slice(1).map(s => s.split(" "))
      votes.forEach(function(a){
        showVote(a[1],a[0])
      })
    }
    if(info.startsWith("The proposed government is: ")){
      proped=info.split("The proposed government is: ")[1].split(" as Chancellor")[0].split(" as President and ")
      $PLAYERLIST$.forEach(function(a){
        document.getElementById(a+"img").className = ""
      })
      document.getElementById(proped[0] + "img").className = "presidentglow"
      document.getElementById(proped[1] + "img").className = "chancellorglow"
    }
    if(info.startsWith("The new government is: ")){
      gov=info.split("The new government is: ")[1].split(" as Chancellor")[0].split(" as President and ")
      $PLAYERLIST$.forEach(function(a){
        document.getElementById(a+"img").className = ""
      })
      document.getElementById(gov[0] + "img").className = "presidentglow"
      document.getElementById(gov[1] + "img").className = "chancellorglow"
    }
    chatLog("\nSERVER: " + e.data.split("|")[1])
  }
}

document.getElementById("chatsubmit").onclick = function(){
  sock.send("Chat|" + $USERNAME$ + "|" + document.getElementById("chatinput").value)
  document.getElementById("chatinput").value = ""
}

document.getElementById("sharddiv").style.display = 'none'
document.getElementById("shardsubmit").onclick = function(){sock.send(document.getElementById("shardselect").value)}

document.getElementById("inputbutton").onclick = function(){
  sock.send("Hi! I am " + document.getElementById("inputbox").value)
}
document.getElementById("JAbutton").onclick = function(){sock.send("Resp|Ja");showVotebox(false)}
document.getElementById("NEINbutton").onclick = function(){sock.send("Resp|Nein");showVotebox(false)}
showVotebox(false)

window.onbeforeunload = function(){sock.close()}
showConnected(false)

showCardbox(false,[])

function showConnected(onOff){
  document.getElementById("connectiontext").innerHTML = onOff ? 'On shard ' + $SERVERNAME$ + ' as ' + $USERNAME$ : ''
  document.getElementById("connectionindicator").src = onOff ? "connected.png" : "disconnected.png"
}
function showVotebox(onOff){
  document.getElementById("votebox").style.display = onOff ? 'block' : 'none'
}
function removePlayer(name){
  $PLAYERLIST$.splice($PLAYERLIST$.indexOf(name),1)
  updateFascBoard()
  document.getElementById(name + "div").remove()
}
function showVote(name, vote){
  image = ""
  switch (vote){
    case "Ja":
      image = "ja_card.png"
      break
    case "Nein":
      image = "nein_card.png"
      break
  }
  var newimage = document.createElement("img")
  newimage.src = image
  newimage.className = "voteIndicator"
  document.getElementById(name + "div").appendChild(newimage)
  setTimeout(function(){newimage.remove()},5000)
}
  
function addPlayer(name, filename, change = false){
  filename = filename || "player.png"
  $PLAYERLIST$.push(name)
  updateFascBoard()
  var pb = document.getElementById("playerbox")

  var olddiv = document.getElementById(name + "div")
  var newdiv = document.createElement("div")
  newdiv.style.float = "left"
  newdiv.className = "playerDiv"

  var newplayer = document.createElement("img")
  newplayer.onclick = function(){
    if(allowPlayerClick){
      allowPlayerClick=false
      sock.send("Resp|" + this.id.slice(0,-3))
    }
  }
  newplayer.id = name+"img"
  newplayer.src = filename
  newplayer.width = "150"
  newplayer.height = "200"

  var newtext = document.createElement("p")
  newtext.id = name + "text"
  newtext.innerHTML = name
  newtext.className = "playerText"
  
  newdiv.appendChild(newplayer)
  newdiv.appendChild(newtext)

  if(change){
    pb.replaceChild(newdiv, olddiv)
    newdiv.id=name+"div"
    return
  }
  pb.appendChild(newdiv)
  newdiv.id=name+"div"
}
function setCardcount(lib,fasc){
 document.getElementById("liberaloverlay").src = "liberaloverlay" + lib + ".png"
 document.getElementById("fascistoverlay").src = "fascistoverlay" + fasc+ ".png"
}

function showCardbox(onOff,cardlist){
  document.getElementById("cardbox").innerHTML = '';
  for(i in cardlist){
    newcard = document.createElement("img")
    newcard.value = cardlist[i]
    newcard.src = cardlist[i].split(" ")[1].toLowerCase()+"policy.png"
    newcard.onclick = function(){
      sock.send("Resp|" + this.value)
      showCardbox(false,[])
    }
    document.getElementById("cardbox").appendChild(newcard)
  }
  document.getElementById("cardbox").style.display = onOff ? 'block' : 'none'
}
function processShards(data){
  return data.split("\n").slice(1).map(function(e){return e.split(": ")})
}
function chatLog(message){
  chatbox = document.getElementById("chatbox")
  chatbox.innerHTML += message
  chatbox.scrollTop = chatbox.scrollHeight;
}
function setPlayerRole(role){
  document.getElementById("playerroletext").innerHTML = "You are " + role
  document.getElementById("playerroleimage").src = role + ".png"
}
function updateFascBoard(){
  switch ($PLAYERLIST$.length){
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
      document.getElementById("fascistboard").src = "fascistboard5.png"
      break
    case 7:
    case 8:
      document.getElementById("fascistboard").src = "fascistboard7.png"
      break
    case 9:
    case 10:
      document.getElementById("fascistboard").src = "fascistboard9.png"
      break
  }
}
