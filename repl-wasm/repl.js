// ------------------------------------------------------------
// Left pane. The output/results tabs
// ------------------------------------------------------------
let plotCount = 0;


// Console tab and pane for text output from lisp program

const btn = document.createElement('button');
btn.className = 'tab-btn active'; // associates with a style from the css ?
btn.dataset.tab = 'console';
btn.addEventListener('click', () => switchTab('console'));
const labelEl = document.createElement('span');
labelEl.textContent = "Console";
btn.appendChild(labelEl);
document.getElementById('output-tab-bar').appendChild(btn);

const consolePane = document.createElement('div');
consolePane.id = 'output-tab-console';
consolePane.className = 'tab-pane active';
document.getElementById('output-tab-contents').appendChild(consolePane);

function switchTab(id) {

  // Set active on the on the button switched to and remove active
  // from all others.
  document.querySelectorAll('#output-tab-bar .tab-btn').forEach(b => {
    b.classList.toggle('active', b.dataset.tab === id);
  });

  // Hide all panes but the one selected!
  document.querySelectorAll('#output-tab-contents .tab-pane').forEach(p => {
    p.classList.toggle('active', p.id === 'output-tab-' + id);
  });
}
function closeTab(id) {
  const btn  = document.querySelector('#output-tab-bar .tab-btn[data-tab="' + id + '"]');
  const pane = document.getElementById('output-tab-' + id);
  const wasActive = btn && btn.classList.contains('active');
  if (btn)  btn.remove();
  if (pane) pane.remove();
  if (wasActive) switchTab('console');
}

// ------------------------------------------------------------
// Right pane, the Editor tabs
// ------------------------------------------------------------
let editorTabs    = [];
let activeEditor  = null;
let editorTabSeq  = 0;

function createEditorTab(name) {
  editorTabSeq++;
  const id = 'et' + editorTabSeq;

  const btn = document.createElement('button');
  btn.className   = 'tab-btn';
  btn.dataset.tab = id;
  btn.addEventListener('click', () => switchEditorTab(id));

  const labelEl = document.createElement('span');
  labelEl.textContent = name;
  labelEl.addEventListener('dblclick', e => {
    e.stopPropagation();
    const n = prompt('Rename tab:', labelEl.textContent);
    if (n !== null && n.trim()) {
      labelEl.textContent = n.trim();
      tab.filename = n.trim();
    }
  });

  const closeEl = document.createElement('span');
  closeEl.className   = 'tab-close';
  closeEl.textContent = '\u2297';
  closeEl.addEventListener('click', e => { e.stopPropagation(); closeEditorTab(id); });

  btn.appendChild(labelEl);
  btn.appendChild(closeEl);
  document.getElementById('editor-tab-bar').insertBefore(
    btn, document.getElementById('btn-new-editor-tab'));

  const pane     = document.createElement('div');
  pane.className = 'editor-pane';
  const textarea = document.createElement('textarea');
  pane.appendChild(textarea);
  document.getElementById('editor-tab-contents').appendChild(pane);

  const cm = CodeMirror.fromTextArea(textarea, {
    mode: 'scheme',
    theme: 'dracula',
    lineNumbers: true,
    matchBrackets: true,
    autoCloseBrackets: true,
    indentUnit: 2,
    tabSize: 2,
    indentWithTabs: false,
    electricChars: true,
    smartIndent: true,
    extraKeys: {
      'Tab': cm => {
        const cur = cm.getCursor();
        const line = cm.getLine(cur.line);
        const beforeCursor = line.slice(0, cur.ch);
        if (beforeCursor.trim() === '') {
          cm.execCommand('indentAuto');
        } else {
          cm.replaceSelection('\t');
        }
      },
      'Shift-Tab': cm => cm.execCommand('indentLess'),
    }
  });
  cm.setSize('100%', '100%');

  const tab = { id, btn, pane, cm, labelEl, filename: null };
  editorTabs.push(tab);
  switchEditorTab(id);
  return tab;
}

function switchEditorTab(id) {
  editorTabs.forEach(t => {
    const active = t.id === id;
    t.btn.classList.toggle('active', active);
    t.pane.classList.toggle('active', active);
    if (active) {
      activeEditor = t;
      t.cm.refresh();
    }
  });
}

function closeEditorTab(id) {
  if (editorTabs.length <= 1) return;
  const idx = editorTabs.findIndex(t => t.id === id);
  if (idx < 0) return;
  const tab = editorTabs[idx];
  tab.btn.remove();
  tab.pane.remove();
  editorTabs.splice(idx, 1);
  if (activeEditor && activeEditor.id === id) {
    switchEditorTab(editorTabs[Math.max(0, idx - 1)].id);
  }
}

document.getElementById('btn-new-editor-tab').addEventListener('click', () => {
  const n = prompt('Tab name:', 'untitled');
  if (n !== null) createEditorTab(n.trim() || 'untitled');
});

createEditorTab('untitled');

const fileInput = document.getElementById('file-input');
document.getElementById('btn-open').addEventListener('click', () => fileInput.click());
fileInput.addEventListener('change', () => {
  const file = fileInput.files[0];
  if (!file) return;
  const reader = new FileReader();
  reader.onload = e => {
    createEditorTab(file.name);
    activeEditor.cm.setValue(e.target.result);
    activeEditor.filename = file.name;
  };
  reader.readAsText(file);
  fileInput.value = '';
});

function downloadFile(filename, content) {
  const blob = new Blob([content], { type: 'text/plain' });
  const url  = URL.createObjectURL(blob);
  const a    = document.createElement('a');
  a.href     = url;
  a.download = filename;
  a.click();
  URL.revokeObjectURL(url);
}

document.getElementById('btn-save').addEventListener('click', () => {
  if (!activeEditor) return;
  const content  = activeEditor.cm.getValue();
  const filename = activeEditor.filename || 'untitled.lisp';
  downloadFile(filename, content);
});

LispBM().then(lbm => {
  const btnEval     = document.getElementById('btn-eval');
  const btnLoad     = document.getElementById('btn-load');
  const status      = document.getElementById('status');

  function appendOutput(text) {
    consolePane.textContent += text;
    if (consolePane.classList.contains('active')) {
      consolePane.scrollTop = consolePane.scrollHeight;
    }
  }

  function pollOutput() {
    const text = lbm.ccall('lbm_wasm_get_output', 'string', [], []);
    if (text && text.length > 0) {
      appendOutput(text);
      lbm.ccall('lbm_wasm_clear_output', null, [], []);
    }
  }

  window.createPlotTab = function(buf, nbytes, title) {
    //const ptr    = lbm.ccall('lbm_wasm_buf_ptr', 'number', ['number'], [slot]);
    const nFloat = (nbytes / 4) | 0;
      
    const floats = new Float32Array(lbm.HEAP8.buffer, buf, nFloat);
    const ys     = Array.from(floats);
    const xs     = Array.from({length: ys.length}, (_, i) => i);

    plotCount++;
    const id    = 'plot-' + plotCount;
    const label = (title && title.length) ? title : ('Plot ' + plotCount);

    const btn = document.createElement('button');
    btn.className   = 'tab-btn';
    btn.dataset.tab = id;
    btn.addEventListener('click', () => switchTab(id));
    const labelEl = document.createElement('span');
    labelEl.textContent = label;
    const closeEl = document.createElement('span');
    closeEl.className   = 'tab-close';
    closeEl.textContent = '\u2297';
    closeEl.addEventListener('click', e => { e.stopPropagation(); closeTab(id); });
    btn.appendChild(labelEl);
    btn.appendChild(closeEl);
    document.getElementById('output-tab-bar').appendChild(btn);

    const pane = document.createElement('div');
    pane.id        = 'output-tab-' + id;
    pane.className = 'tab-pane plot-pane';
    document.getElementById('output-tab-contents').appendChild(pane);

    switchTab(id);

    const rect = document.getElementById('output-tab-contents').getBoundingClientRect();
    const w    = Math.max(rect.width  - 16, 300);
    const h    = Math.max(rect.height - 16, 200);

    new uPlot({
      title:  label,
      width:  w,
      height: h,
      series: [
        {},
        { label: 'value', stroke: '#4ec9b0', width: 2, fill: 'rgba(78,201,176,0.08)' }
      ],
      axes: [
        { stroke: '#666', grid: { stroke: '#222' }, ticks: { stroke: '#222' } },
        { stroke: '#666', grid: { stroke: '#222' }, ticks: { stroke: '#222' } },
      ],
      scales: { x: { time: false } },
      cursor: { stroke: '#569cd6', width: 1 },
    }, [xs, ys], pane);
  };

  const SERIES_COLORS = ['#4ec9b0', '#569cd6', '#ce9178', '#dcdcaa', '#c586c0', '#f44747', '#b5cea8', '#9cdcfe'];

  window.createMultiPlotTab = function(slotsJson, title) {

    plotCount++;
    const id    = 'plot-' + plotCount;
    const label = (title && title.length) ? title : ('Plot ' + plotCount);

    const btn = document.createElement('button');
    btn.className   = 'tab-btn';
    btn.dataset.tab = id;
    btn.addEventListener('click', () => switchTab(id));
    const labelEl = document.createElement('span');
    labelEl.textContent = label;
    const closeEl = document.createElement('span');
    closeEl.className   = 'tab-close';
    closeEl.textContent = '\u2297';
    closeEl.addEventListener('click', e => { e.stopPropagation(); closeTab(id); });
    btn.appendChild(labelEl);
    btn.appendChild(closeEl);
    document.getElementById('output-tab-bar').appendChild(btn);

    const pane = document.createElement('div');
    pane.id        = 'output-tab-' + id;
    pane.className = 'tab-pane plot-pane';
    document.getElementById('output-tab-contents').appendChild(pane);

    switchTab(id);

    const rect = document.getElementById('output-tab-contents').getBoundingClientRect();
    const w    = Math.max(rect.width  - 16, 300);
    const h    = Math.max(rect.height - 16, 200);

    const bufs = JSON.parse(slotsJson);
    let maxLen = 0;
    const yArrays = bufs.map(({ptr, nbytes}) => {
      const nFloat = (nbytes / 4) | 0;
      const ys     = Array.from(new Float32Array(lbm.HEAP8.buffer, ptr, nFloat));
      if (ys.length > maxLen) maxLen = ys.length;
      return ys;
    });
    const xs = Array.from({length: maxLen}, (_, i) => i);

    const series = [{}];
    bufs.forEach((_, i) => {
      series.push({ label: 'series ' + i, stroke: SERIES_COLORS[i % SERIES_COLORS.length], width: 2 });
    });

    new uPlot({
      title:  label,
      width:  w,
      height: h,
      series,
      axes: [
        { stroke: '#666', grid: { stroke: '#222' }, ticks: { stroke: '#222' } },
        { stroke: '#666', grid: { stroke: '#222' }, ticks: { stroke: '#222' } },
      ],
      scales: { x: { time: false } },
      cursor: { stroke: '#569cd6', width: 1 },
    }, [xs, ...yArrays], pane);
  };

  console.log('calling lbm_wasm_init...');
  const ok = lbm.ccall('lbm_wasm_init', 'number', [], []);
  console.log('lbm_wasm_init returned:', ok);
  if (!ok) {
    appendOutput('Failed to initialise LispBM.\n');
    status.textContent = 'Init failed';
    return;
  }

  btnEval.disabled = false;
  btnLoad.disabled = false;
  const input = document.getElementById('input');
  input.disabled = false;
  input.focus();
  status.textContent = 'Ready';

  function loop() {
    const deadline = performance.now() + 8;
    try {
      while (performance.now() < deadline) {
        lbm.ccall('lbm_wasm_run', null, ['number'], [100]);
      }
    } catch(e) {
      appendOutput('CRASH in step: ' + e + '\n');
      status.textContent = 'Crashed';
      return;
    }
    try {
      pollOutput();
    } catch(e) {
      appendOutput('CRASH in pollOutput: ' + e + '\n');
      status.textContent = 'Crashed';
      return;
    }
    setTimeout(loop, 0);
  }
  setTimeout(loop, 0);

  function evalExpr() {
    const code = input.value.trim();
    if (!code) return;
    appendOutput('# ' + code + '\n');
    lbm.ccall('lbm_wasm_eval', null, ['string'], [code]);
    input.value = '';
  }

  function loadEditor() {
    if (!activeEditor) return;
    const code = activeEditor.cm.getValue().trim();
    if (!code) return;
    lbm.ccall('lbm_wasm_eval_program', null, ['string'], [code]);
  }

  btnEval.addEventListener('click', evalExpr);
  input.addEventListener('keydown', e => { if (e.key === 'Enter') evalExpr(); });
  btnLoad.addEventListener('click', loadEditor);

}).catch(err => {
  document.getElementById('status').textContent = 'Error: ' + err;
});
