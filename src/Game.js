import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import { joinResult, joinResultAux } from './util';
import { numberToColor } from './util';
import Loading from './Loading';
let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [score, setScore] = useState(0);
  const [path, setPath] = useState([]);
  const [waiting, setWaiting] = useState(false);
  //Si se esta haciendo un path (Se usa para el valor) 
  const [valorPath, setValorPath] = useState(0);
  const [isActive, setIsActive] = useState(false);
  useEffect(() => {
    // This is executed just once, after the first render.
    PengineClient.init(onServerReady);
  }, []);

//Usado para display del path
const displayValue = isActive ? valorPath : score;

  /**
   * Called when the server was successfully initialized
   */
  function onServerReady(instance) {
    pengine = instance;
    const queryS = 'init(Grid, NumOfColumns)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setNumOfColumns(response['NumOfColumns']);
      }
    });
  }

  /**
   * Called while the user is drawing a path in the grid, each time the path changes.
   */
  function onPathChange(newPath) {
    // No effect if waiting.
    if (waiting) {
      return;
    }
    setPath(newPath);
    console.log(JSON.stringify(newPath));
    setIsActive(true); //Cambia el valor en el return
    setValorPath(joinResult(newPath, grid, numOfColumns));
    if(newPath.length === 0) {
      setIsActive(false);
    }
  }

  /**
   * Called when the user finished drawing a path in the grid.
   */
  function onPathDone() {
    
    /**
   *Build Prolog query, which will be like:
  
    join([
          64,4,64,32,16,
          64,8,16,2,32,
          2,4,64,64,2,
          2,4,32,16,4,
          16,4,16,16,16,
          16,64,2,32,32,
          64,2,64,32,64,
          32,2,64,32,4
          ], 
          5, 
          [[2, 0], [3, 0], [4, 1], [3, 1], [2, 1], [1, 1], [1, 2], [0, 3]],
          RGrids
        );
   */
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);
    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";
    setValorPath(0);
    setWaiting(true);
    setIsActive(false);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        setPath([]);
        animateEffect(response['RGrids']);
      } else {
        setWaiting(false);
      }
    });
  }

  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals.
   * @param {number[][]} rGrids a sequence of grids.
   */
  function animateEffect(rGrids) {
    
   //let time1=time;
    setGrid(rGrids[0]);
    const restRGrids = rGrids.slice(1);
    if (restRGrids.length > 0) {
      setTimeout(() => {
        animateEffect(restRGrids);
      }, 500);
    } else {
      setWaiting(false);
    }
  }


  function handleClick() {
    if (waiting) {
      return;
    }
    const gridS = JSON.stringify(grid);
    const queryS = "booster(" + gridS + "," + numOfColumns + ", RGrids)";

    setWaiting(true);
    setIsActive(false);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        setPath([]);
        animateEffect(response['RGrids']);
      } else {
        setWaiting(false);
      }
    });
  }

  function handleClickMovidaMaxima() {
    if (waiting) {
      return;
    }
    const gridS = JSON.stringify(grid);
    const queryS = "movida_maxima(" + gridS + "," + numOfColumns + ", RGrids)";

    setWaiting(true);
    setIsActive(false);
    pengine.query(queryS, (success, response) => {
      if (success) {
        
        setPath(response['RGrids']);
        console.log(JSON.stringify(response['RGrids']));
        setIsActive(true); //Cambia el valor en el return
        setValorPath(joinResult(response['RGrids'], grid, numOfColumns));
        setWaiting(false);
      } else {
        setWaiting(false);
      }
    });
  }
  
  function handleClickMaximoAdyacente(){
    if (waiting) {
      return;
    }
    const gridS = JSON.stringify(grid);
    const queryS = "maximo_adyacente(" + gridS + "," + numOfColumns + ", RGrids)";

    setWaiting(true);
    setIsActive(false);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setPath(response['RGrids']);
        console.log(JSON.stringify(response['RGrids']));
        setIsActive(true); //Cambia el valor en el return
        setValorPath(joinResult(response['RGrids'], grid, numOfColumns));
        setWaiting(false);
      } else {
        setWaiting(false);
      }
    });
  } 

  if (grid === null) {
    return null;
  }
  return (
      <div className="game">
          {waiting && <Loading />} 
        <div className="header">
          <div className={isActive ? 'squareScore active' : 'score'} style={isActive ? { backgroundColor: numberToColor(displayValue) } : {}}>
              {displayValue}
        
          </div>
        </div>     
      <div>
      
      <body>
        <button id="booster" onClick={handleClick}>BOOSTER</button>
        <button id="MovidaMaxima" onClick={handleClickMovidaMaxima}>MOVIDA MAXIMA</button>
        <button id="MaximoAdyacente" onClick={handleClickMaximoAdyacente}>MAXIMO ADYACENTE</button>
      </body>
      
      </div>
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
      />
    </div>
    
  );
}

export default Game;