/**
 * This file will automatically be loaded by webpack and run in the "renderer" context.
 * To learn more about the differences between the "main" and the "renderer" context in
 * Electron, visit:
 *
 * https://electronjs.org/docs/tutorial/application-architecture#main-and-renderer-processes
 *
 * By default, Node.js integration in this file is disabled. When enabling Node.js integration
 * in a renderer process, please be aware of potential security implications. You can read
 * more about security risks here:
 *
 * https://electronjs.org/docs/tutorial/security
 *
 * To enable Node.js integration in this file, open up `main.js` and enable the `nodeIntegration`
 * flag:
 *
 * ```
 *  // Create the browser window.
 *  mainWindow = new BrowserWindow({
 *    width: 800,
 *    height: 600,
 *    webPreferences: {
 *      nodeIntegration: true
 *    }
 *  });
 * ```
 */

import './index.css';

console.log('👋 This message is being logged by "renderer.js", included via webpack');

async function populateTable() {
    const data = await window.api.fetchData(); // Fetch data using the exposed API

    console.log('Data:', data);
  
    const table = document.createElement('table');
    table.border = '1';
    table.width = '100%';
    table.cellPadding = '5';
    table.cellSpacing = '0';
  
    const thead = document.createElement('thead');
    const tbody = document.createElement('tbody');
  
    const headerRow = document.createElement('tr');
    for (let key in data[0]) {
      const th = document.createElement('th');
      th.appendChild(document.createTextNode(key));
      headerRow.appendChild(th);
    }
    thead.appendChild(headerRow);
  
    for (let record of data) {
      const row = document.createElement('tr');
      for (let key in record) {
        const cell = document.createElement('td');
        cell.appendChild(document.createTextNode(record[key]));
        row.appendChild(cell);
      }
      tbody.appendChild(row);
    }
  
    table.appendChild(thead);
    table.appendChild(tbody);
  
    document.body.appendChild(table);
  }
  
  // Call the function when the content is loaded
  window.addEventListener('DOMContentLoaded', () => {
    populateTable();
  });

