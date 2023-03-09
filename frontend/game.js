// Stores the HTML representation of the board
var gameRoom = [];
var room = "start";

// Current player data
var playerData =
  { energy:   20,
    score:    0,
    inventory: []
  }
var positionX;
var positionY;

// Game state
var playing = true;

// Handle user input from the keyboard to move the player
function keyEvent(event) {
  if (playing) {
    switch (event.key) {
      case "ArrowLeft":
        move(-1, 0);
        var player = document.getElementsByClassName("player")[0];
        player.style.backgroundImage = "url('frontend/images/chanA.png')";
        break;
      case "ArrowRight":
        move(1, 0);
        var player = document.getElementsByClassName("player")[0];
        player.style.backgroundImage = "url('frontend/images/chanB.png')";
        break;
      case "ArrowUp":
        move(0, -1);
        break;
      case "ArrowDown":
        move(0, 1);
        break;
    }
  }
}

// Action a movement
function move(x, y) {
  var size = gameRoom.length;

  // New position
  var newPosX = positionX + x;
  var newPosY = positionY + y;

  // We cannot move outside boundaries or onto rocks
  if (newPosX < size && newPosX >= 0 &&
    newPosY < size && newPosY >= 0 &&
    gameRoom[newPosY][newPosX].className.substring(0,4) != "rock"
    && gameRoom[newPosY][newPosX].className.substring(0,4) != "tree") {
    // Valid move

    // Action any opening of doors
    if (gameRoom[newPosY][newPosX].className == "door") {
      // setup game board for new room
      if (checkInventory(objects[newPosY][newPosX].reqs)) {
          // update in the game map the current object v iew
          gameMap[room] = objects;
          // get new room name and update objects
          room = objects[newPosY][newPosX].snd;
          objects = gameMap[room];
          // reload map
          setup(objects);
          var oldSize = size;
          size = objects.length;
          // flip edge to go through opposite side
          if (newPosX == 0) { newPosX = size - 2; }
          else if (newPosX == oldSize - 1) { newPosX = 1; }
          if (newPosY == 0) { newPosY = size - 2; }
          else if (newPosY == oldSize - 1) { newPosY = 1; }

          positionX = newPosX;
          positionY = newPosY;
          updateStatus();
          // visually move the player last
          gameRoom[newPosY][newPosX].className = "player";
      } else {
        var warning = document.getElementById("warning");
        warning.innerHTML = "Needs keys: " + objects[newPosY][newPosX].reqs
        warning.style.color = "red"
        setTimeout(function(){ warning.style.color = "#eee"}, 4000);
        return;
      }


    } else {

    playerData.energy--;

    // Run the function associated with this kind of tile
    if (actions[gameRoom[newPosY][newPosX].className]) {
      playerData = actions[gameRoom[newPosY][newPosX].className](playerData);
    }

    // Update player position and set old cell to empty
    gameRoom[positionY][positionX].className = "none";
    objects[positionY][positionX] = {fst: "none", snd: null}
    gameRoom[positionY][positionX].innerHTML = "";
    gameRoom[positionY][positionX].style.backgroundImage = "";
    positionX = newPosX;
    positionY = newPosY;
    updateStatus();

    // Check win
    if (gameRoom[positionY][positionX].className == "home") {
      // WIN!
      document.getElementById("outcome").innerHTML =
        "You won! Made it home with final score: " + playerData.score;
      playing = false;

    } else {
      // LOSE
      if (playerData.energy == 0) {
        document.getElementById("outcome").innerHTML =
          "GAME OVER! Out of energy";
        document.getElementById("outcome").style.color = "red";
        playing = false;
      }
    }
    // visually move the player last
    gameRoom[newPosY][newPosX].className = "player";
    }
  }
}

function checkInventory(reqs) {
  var satisfies = true;
  for (var i = 0; i < reqs.length; i++) {
    satisfies = satisfies && playerData.inventory.includes(reqs[i]);
  }
  return satisfies;
}

// Colours for the top bar
var barColours =
["red", "red", "red", "red", "rgb(255,50,0)","rgb(255,50,0)","rgb(255,75,0)",
"rgb(255,100,0)","rgb(255,125,0)","rgb(255,150,0)","rgb(255,150,0)","rgb(255,175,0)",
,"rgb(255,200,0)","rgb(255,200,0)","rgb(245,200,0)","rgb(225,200,0)","rgb(210,200,0)"
,"rgb(190,200,0)","rgb(170,200,0)","rgb(120,200,0)","rgb(100,200,0)","rgb(40,200,0)"
,"rgb(40,200,0)","rgb(40,200,0)","rgb(40,200,0)","rgb(40,200,0)"]

// Update the player status after every move
function updateStatus() {
  document.getElementById("energy").style.width = (playerData.energy*10) + "px";
  if (playerData.energy < 25) {
    document.getElementById("energy").style.backgroundColor = barColours[playerData.energy];
  } else {
    document.getElementById("energy").style.backgroundColor = "rgb(40,200,0)";
  }
  document.getElementById("score").innerHTML = playerData.score;
  if (playerData.energy < (positionX + positionY)) {
    document.getElementById("energy").style.color = "red";
  } else {
    document.getElementById("energy").style.color = "black";
  }
  // update inventory status
  var inventory = document.getElementById("inventory");
  inventory.innerHTML = "";
  for (var i = 0; i < playerData.inventory.length; i++) {
    let k = document.createElement("img");
    k.className = "inventoryItem"
    k.src = "frontend/images/key" + playerData.inventory[i].toLowerCase() + ".png"
    inventory.appendChild(k);
  }
}

// Create the game board for the current room
function setup(objects) {
  gameRoom = []
  var size = objects.length;
  var board = document.getElementById("board");
  // remove all existing elements
  board.innerHTML = "";

  for (var i = 0; i < size; i++) {

    let htmlRow = document.createElement("tr");
    board.appendChild(htmlRow);
    let row = []

    for (var j = 0; j < objects[i].length; j++) {
      let cell = document.createElement("td");
      cell.className = objects[i][j].fst;
      htmlRow.appendChild(cell)
      row.push(cell)
    }
    gameRoom.push(row)
  }

  // Listener
  document.body.addEventListener("keydown", keyEvent);
  updateStatus();
}

function cons(x, xs) {
  let ys = [x];
  return (ys.concat(xs));
}

function initPlayerPos(objects) {
  var size = objects.length;
  // Player
  gameRoom[size-1][size-1].className = "player";
  positionX = size - 1;
  positionY = size - 1;
}