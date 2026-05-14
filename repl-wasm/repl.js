// ------------------------------------------------------------
// GPIO / ADC simulation state
// ------------------------------------------------------------
const gpioInputVal  = {}; // set by sim-gpio-write, read by gpio-read
const gpioOutputVal = {}; // set by gpio-write, read by sim-gpio-read
const adcSimVal     = {}; // set by sim-adc-set, read by sim-adc-get

window.gpioSetMode  = function(pin, mode) {
  const isOut = mode === 'pin-mode-out' || mode === 'pin-mode-od' ||
                mode === 'pin-mode-od-pu' || mode === 'pin-mode-od-pd';
  if (isOut) { if (gpioOutputVal[pin] === undefined) gpioOutputVal[pin] = 0; }
  else        { if (gpioInputVal[pin]  === undefined) gpioInputVal[pin]  = 0; }
};
window.gpioWrite    = function(pin, value)  { gpioOutputVal[pin] = value; };
window.gpioRead     = function(pin)         { return gpioInputVal[pin]  !== undefined ? gpioInputVal[pin]  : 0; };
window.simGpioWrite = function(pin, val)    { gpioInputVal[pin]  = val; };
window.simGpioRead  = function(pin)         { return gpioOutputVal[pin] !== undefined ? gpioOutputVal[pin] : 0; };
window.simAdcSet    = function(pin, val)    { adcSimVal[pin] = val; };
window.simAdcGet    = function(pin)         { return adcSimVal[pin]     !== undefined ? adcSimVal[pin]     : 0.0; };

function createInspectTab() {
  const existing = editorTabs.find(t => t.isSim && t.simLabel === 'GPIO');
  if (existing) { switchEditorTab(existing.id); return; }

  editorTabSeq++;
  const id = 'et' + editorTabSeq;

  const btn = document.createElement('button');
  btn.className = 'tab-btn';
  btn.dataset.tab = id;
  btn.addEventListener('click', () => switchEditorTab(id));
  const labelEl = document.createElement('span');
  labelEl.textContent = 'GPIO';
  const closeEl = document.createElement('span');
  closeEl.className = 'tab-close';
  closeEl.textContent = '⊗';
  closeEl.addEventListener('click', e => { e.stopPropagation(); closeEditorTab(id); });
  btn.appendChild(labelEl);
  btn.appendChild(closeEl);
  document.getElementById('editor-tab-bar').insertBefore(btn, document.getElementById('btn-new-editor-tab'));

  const pane = document.createElement('div');
  pane.className = 'sim-pane sim-value-pane';

  function makeSection(title, colHeaders) {
    const wrap = document.createElement('div');
    wrap.style.marginBottom = '16px';
    const hdr = document.createElement('div');
    hdr.className = 'inspect-section-hdr';
    hdr.textContent = title;
    wrap.appendChild(hdr);
    const tbl = document.createElement('table');
    tbl.className = 'sim-value-table';
    const thead = document.createElement('thead');
    const hrow = document.createElement('tr');
    colHeaders.forEach(h => {
      const th = document.createElement('th'); th.textContent = h; hrow.appendChild(th);
    });
    thead.appendChild(hrow);
    tbl.appendChild(thead);
    const tbody = document.createElement('tbody');
    tbl.appendChild(tbody);
    wrap.appendChild(tbl);
    pane.appendChild(wrap);
    return tbody;
  }

  const gpioTbody = makeSection('GPIO', ['Pin', 'Input', 'Output']);
  const adcTbody  = makeSection('ADC',  ['Pin', 'Value']);

  const gpioRows = {}; // pin -> { inputEl, outputEl }
  const adcRows  = {}; // pin -> { valEl }

  function addGpioRow(pin) {
    if (gpioRows[pin]) return;
    const tr = document.createElement('tr');
    const tdPin = document.createElement('td');
    tdPin.className = 'sim-value-key';
    tdPin.textContent = pin;
    tr.appendChild(tdPin);

    const tdIn = document.createElement('td');
    const inputEl = document.createElement('input');
    inputEl.type = 'number'; inputEl.className = 'sim-value-input';
    inputEl.value = gpioInputVal[pin] !== undefined ? gpioInputVal[pin] : 0;
    inputEl.addEventListener('change', () => { window.simGpioWrite(pin, Number(inputEl.value)); });
    tdIn.appendChild(inputEl);
    tr.appendChild(tdIn);

    const tdOut = document.createElement('td');
    tdOut.style.color = '#888';
    const outputEl = document.createElement('span');
    outputEl.textContent = gpioOutputVal[pin] !== undefined ? gpioOutputVal[pin] : '—';
    tdOut.appendChild(outputEl);
    tr.appendChild(tdOut);

    gpioTbody.appendChild(tr);
    gpioRows[pin] = { inputEl, outputEl };
  }

  function addAdcRow(pin) {
    if (adcRows[pin]) return;
    const tr = document.createElement('tr');
    const tdPin = document.createElement('td');
    tdPin.className = 'sim-value-key';
    tdPin.textContent = pin;
    tr.appendChild(tdPin);

    const tdVal = document.createElement('td');
    const valEl = document.createElement('input');
    valEl.type = 'number'; valEl.step = 'any'; valEl.className = 'sim-value-input';
    valEl.value = adcSimVal[pin] !== undefined ? adcSimVal[pin] : 0;
    valEl.addEventListener('change', () => { window.simAdcSet(pin, Number(valEl.value)); });
    tdVal.appendChild(valEl);
    tr.appendChild(tdVal);

    adcTbody.appendChild(tr);
    adcRows[pin] = { valEl };
  }

  function makeAddRow(tbody, placeholder, onAdd) {
    const tr = document.createElement('tr');
    const tdPin = document.createElement('td');
    const pinInp = document.createElement('input');
    pinInp.type = 'text'; pinInp.placeholder = placeholder;
    pinInp.className = 'sim-value-key-inp'; pinInp.style.width = '100%';
    tdPin.appendChild(pinInp);
    tr.appendChild(tdPin);
    const tdBtn = document.createElement('td');
    tdBtn.colSpan = 99;
    const addBtn = document.createElement('button');
    addBtn.textContent = '+ Add';
    addBtn.className = 'sim-add-btn';
    addBtn.addEventListener('click', () => {
      const pin = pinInp.value.trim();
      if (!pin) return;
      onAdd(pin);
      pinInp.value = '';
    });
    tdBtn.appendChild(addBtn);
    tr.appendChild(tdBtn);
    tbody.appendChild(tr);
  }

  makeAddRow(gpioTbody, 'pin or number', pin => {
    window.simGpioWrite(pin, 0);
    addGpioRow(pin);
  });
  makeAddRow(adcTbody, 'pin or number', pin => {
    window.simAdcSet(pin, 0.0);
    addAdcRow(pin);
  });

  function refresh() {
    Object.keys(gpioInputVal).forEach(addGpioRow);
    Object.keys(gpioOutputVal).forEach(addGpioRow);
    Object.keys(adcSimVal).forEach(addAdcRow);
    Object.entries(gpioRows).forEach(([pin, row]) => {
      if (document.activeElement !== row.inputEl)
        row.inputEl.value = gpioInputVal[pin] !== undefined ? gpioInputVal[pin] : 0;
      row.outputEl.textContent = gpioOutputVal[pin] !== undefined ? gpioOutputVal[pin] : '—';
    });
    Object.entries(adcRows).forEach(([pin, row]) => {
      if (document.activeElement !== row.valEl)
        row.valEl.value = adcSimVal[pin] !== undefined ? adcSimVal[pin] : 0;
    });
  }

  document.getElementById('editor-tab-contents').appendChild(pane);
  const tab = { id, btn, pane, cm: null, labelEl, filename: null, baseUrl: null, isSim: true, simLabel: 'GPIO' };
  editorTabs.push(tab);
  switchEditorTab(id);

  setInterval(() => { if (btn.classList.contains('active')) refresh(); }, 500);
  refresh();
}

// ------------------------------------------------------------
// BMS and Config simulation state
// ------------------------------------------------------------
window.bmsState = {
  'v-tot':            { val: 42.0,  type: 'f32' },
  'v-min':            { val: 3.5,   type: 'f32' },
  'v-max':            { val: 3.7,   type: 'f32' },
  'i-in':             { val: 0.0,   type: 'f32' },
  'i-in-ic':          { val: 0.0,   type: 'f32' },
  'ah-cnt':           { val: 0.0,   type: 'f32' },
  'ah-cnt-chg-total': { val: 0.0,   type: 'f32' },
  'wh-cnt':           { val: 0.0,   type: 'f32' },
  'wh-cnt-chg-total': { val: 0.0,   type: 'f32' },
  'soc':              { val: 100.0, type: 'f32' },
  'soh':              { val: 100.0, type: 'f32' },
  'temp-adc-0':       { val: 25.0,  type: 'f32' },
  'temp-adc-1':       { val: 25.0,  type: 'f32' },
  'temp-adc-2':       { val: 25.0,  type: 'f32' },
  'num-cell-groups':  { val: 12,    type: 'i'   },
  'cell-num':         { val: 12,    type: 'i'   },
  'balancing':        { val: 0,     type: 'i'   },
  'is-balancing':     { val: 0,     type: 'i'   },
  'can-id':           { val: 10,    type: 'i'   },
};

window.configState = {
  // Limits
  'l-current-min':          { val: -60.0,    type: 'f32' },
  'l-current-max':          { val:  60.0,    type: 'f32' },
  'l-current-min-scale':    { val:   1.0,    type: 'f32' },
  'l-current-max-scale':    { val:   1.0,    type: 'f32' },
  'l-in-current-min':       { val: -30.0,    type: 'f32' },
  'l-in-current-max':       { val:  30.0,    type: 'f32' },
  'l-abs-current-max':      { val: 130.0,    type: 'f32' },
  'l-min-erpm':             { val: -100000.0,type: 'f32' },
  'l-max-erpm':             { val:  100000.0,type: 'f32' },
  'l-erpm-start':           { val:   0.9,    type: 'f32' },
  'l-min-vin':              { val:   8.0,    type: 'f32' },
  'l-max-vin':              { val:  57.0,    type: 'f32' },
  'l-min-duty':             { val:   0.005,  type: 'f32' },
  'l-max-duty':             { val:   0.95,   type: 'f32' },
  'l-watt-min':             { val: -500.0,   type: 'f32' },
  'l-watt-max':             { val:  500.0,   type: 'f32' },
  'l-battery-cut-start':    { val:  36.0,    type: 'f32' },
  'l-battery-cut-end':      { val:  34.0,    type: 'f32' },
  'l-temp-motor-start':     { val:  85.0,    type: 'f32' },
  'l-temp-motor-end':       { val:  95.0,    type: 'f32' },
  'l-temp-accel-dec':       { val:   1.0,    type: 'f32' },
  // BMS limits
  'bms-limit-mode':         { val:  0,       type: 'i'   },
  'bms-t-limit-start':      { val:  45.0,    type: 'f32' },
  'bms-t-limit-end':        { val:  65.0,    type: 'f32' },
  'bms-vmin-limit-start':   { val:   3.3,    type: 'f32' },
  'bms-vmin-limit-end':     { val:   3.0,    type: 'f32' },
  'bms-vmax-limit-start':   { val:   4.1,    type: 'f32' },
  'bms-vmax-limit-end':     { val:   4.2,    type: 'f32' },
  // Motor
  'motor-type':             { val:  2,       type: 'i'   },
  'm-invert-direction':     { val:  0,       type: 'i'   },
  'm-out-aux-mode':         { val:  0,       type: 'i'   },
  'm-motor-temp-sens-type': { val:  0,       type: 'i'   },
  'm-ntc-motor-beta':       { val:  3380.0,  type: 'f32' },
  'm-ptc-motor-coeff':      { val:  0.0,     type: 'f32' },
  'm-ntcx-ptcx-temp-base':  { val:  25.0,    type: 'f32' },
  'm-ntcx-ptcx-res':        { val:  10000.0, type: 'f32' },
  'm-encoder-counts':       { val:  8192,    type: 'i'   },
  'm-sensor-port-mode':     { val:  0,       type: 'i'   },
  'm-fault-stop-time-ms':   { val:  500,     type: 'i'   },
  // SI / System
  'si-motor-poles':         { val:  14,      type: 'i'   },
  'si-gear-ratio':          { val:   1.0,    type: 'f32' },
  'si-wheel-diameter':      { val:   0.083,  type: 'f32' },
  'si-battery-cells':       { val:  12,      type: 'i'   },
  'si-battery-ah':          { val:  18.0,    type: 'f32' },
  'min-speed':              { val: -30.0,    type: 'f32' },
  'max-speed':              { val:  30.0,    type: 'f32' },
  // FOC
  'foc-sensor-mode':        { val:  0,       type: 'i'   },
  'foc-encoder-offset':     { val:  0.0,     type: 'f32' },
  'foc-encoder-inverted':   { val:  0,       type: 'i'   },
  'foc-encoder-ratio':      { val:  1.0,     type: 'f32' },
  'foc-current-kp':         { val:  0.01,    type: 'f32' },
  'foc-current-ki':         { val:  20.0,    type: 'f32' },
  'foc-f-zv':               { val:  40000.0, type: 'f32' },
  'foc-motor-l':            { val:  10.0,    type: 'f32' },
  'foc-motor-ld-lq-diff':   { val:  0.0,     type: 'f32' },
  'foc-motor-r':            { val:  50.0,    type: 'f32' },
  'foc-motor-flux-linkage': { val:  15.0,    type: 'f32' },
  'foc-observer-gain':      { val:   2.0,    type: 'f32' },
  'foc-observer-type':      { val:  0,       type: 'i'   },
  'foc-mtpa-mode':          { val:  0,       type: 'i'   },
  'foc-hfi-amb-mode':       { val:  0,       type: 'i'   },
  'foc-hfi-amb-current':    { val:  2.0,     type: 'f32' },
  'foc-hfi-amb-tres':       { val:  0.02,    type: 'f32' },
  'foc-hfi-voltage-start':  { val:  4.0,     type: 'f32' },
  'foc-hfi-voltage-run':    { val:  2.0,     type: 'f32' },
  'foc-hfi-voltage-max':    { val:  6.0,     type: 'f32' },
  'foc-sl-erpm':            { val:  4000.0,  type: 'f32' },
  'foc-sl-erpm-start':      { val:  1000.0,  type: 'f32' },
  'foc-hall-t0':            { val:  255,     type: 'i'   },
  'foc-hall-t1':            { val:  255,     type: 'i'   },
  'foc-hall-t2':            { val:  255,     type: 'i'   },
  'foc-hall-t3':            { val:  255,     type: 'i'   },
  'foc-hall-t4':            { val:  255,     type: 'i'   },
  'foc-hall-t5':            { val:  255,     type: 'i'   },
  'foc-hall-t6':            { val:  255,     type: 'i'   },
  'foc-hall-t7':            { val:  255,     type: 'i'   },
  'foc-sl-erpm-hfi':        { val:  3000.0,  type: 'f32' },
  'foc-hfi-reset-erpm':     { val:   600.0,  type: 'f32' },
  'foc-openloop-rpm':       { val:   700.0,  type: 'f32' },
  'foc-openloop-rpm-low':   { val:   100.0,  type: 'f32' },
  'foc-sl-openloop-time-lock':  { val: 0.0,  type: 'f32' },
  'foc-sl-openloop-time-ramp':  { val: 0.3,  type: 'f32' },
  'foc-sl-openloop-time':   { val:  0.5,     type: 'f32' },
  'foc-temp-comp':          { val:  0,       type: 'i'   },
  'foc-temp-comp-base-temp':{ val:  25.0,    type: 'f32' },
  'foc-offsets-cal-on-boot':{ val:  0,       type: 'i'   },
  'foc-offsets-cal-mode':   { val:  0,       type: 'i'   },
  'foc-fw-current-max':     { val:  0.0,     type: 'f32' },
  'foc-fw-duty-start':      { val:  0.9,     type: 'f32' },
  'foc-short-ls-on-zero-duty': { val: 0,     type: 'i'   },
  'foc-overmod-factor':     { val:  1.0,     type: 'f32' },
  // App / comms
  'app-to-use':             { val:  0,       type: 'i'   },
  'controller-id':          { val:  0,       type: 'i'   },
  'timeout-msec':           { val:  1000,    type: 'i'   },
  'can-baud-rate':          { val:  2,       type: 'i'   },
  'can-mode':               { val:  1,       type: 'i'   },
  'can-status-rate-1':      { val:  50.0,    type: 'f32' },
  'can-status-msgs-r1':     { val:  0,       type: 'i'   },
  'can-status-rate-2':      { val:  5.0,     type: 'f32' },
  'can-status-msgs-r2':     { val:  0,       type: 'i'   },
  'can-status-rate-hz':     { val:  50.0,    type: 'f32' },
  // PPM
  'ppm-ctrl-type':          { val:  0,       type: 'i'   },
  'ppm-pulse-start':        { val:  1.0,     type: 'f32' },
  'ppm-pulse-end':          { val:  2.0,     type: 'f32' },
  'ppm-pulse-center':       { val:  1.5,     type: 'f32' },
  'ppm-ramp-time-pos':      { val:  0.4,     type: 'f32' },
  'ppm-ramp-time-neg':      { val:  0.2,     type: 'f32' },
  'ppm-hyst':               { val:  0.15,    type: 'f32' },
  // ADC
  'adc-ctrl-type':          { val:  0,       type: 'i'   },
  'adc-ramp-time-pos':      { val:  0.3,     type: 'f32' },
  'adc-ramp-time-neg':      { val:  0.3,     type: 'f32' },
  'adc-thr-hyst':           { val:  0.05,    type: 'f32' },
  'adc-v1-start':           { val:  0.9,     type: 'f32' },
  'adc-v1-end':             { val:  4.1,     type: 'f32' },
  'adc-v1-min':             { val:  0.3,     type: 'f32' },
  'adc-v1-max':             { val:  4.5,     type: 'f32' },
  'pas-current-scaling':    { val:  1.0,     type: 'f32' },
  // Express / WiFi / BLE
  'wifi-mode':              { val:  0,         type: 'i'   },
  'wifi-sta-ssid':          { val:  '',        type: 'str' },
  'wifi-sta-key':           { val:  '',        type: 'str' },
  'wifi-ap-ssid':           { val:  'VESC',    type: 'str' },
  'wifi-ap-key':            { val:  '',        type: 'str' },
  'use-tcp-local':          { val:  0,         type: 'i'   },
  'use-tcp-hub':            { val:  0,         type: 'i'   },
  'tcp-hub-url':            { val:  '',        type: 'str' },
  'tcp-hub-port':           { val:  8000,      type: 'i'   },
  'tcp-hub-id':             { val:  '',        type: 'str' },
  'tcp-hub-pass':           { val:  '',        type: 'str' },
  'ble-mode':               { val:  0,         type: 'i'   },
  'ble-name':               { val:  'VESC',    type: 'str' },
  'ble-pin':                { val:  1234,      type: 'i'   },
  'ble-service-capacity':   { val:  10,        type: 'i'   },
  'ble-chr-descr-capacity': { val:  50,        type: 'i'   },
};

window.eepromState = {};
let eepromRefresh = null;

window.gnssState = {
  'gnss-lat-lon':   { val: [{val: 0.0, type: 'f64'}, {val: 0.0, type: 'f64'}], type: 'list' },
  'gnss-height':    { val: 0.0,  type: 'f32' },
  'gnss-speed':     { val: 0.0,  type: 'f32' },
  'gnss-hdop':      { val: 1.0,  type: 'f32' },
  'gnss-date-time': { val: [{val: 2024, type: 'i32'}, {val: 1, type: 'i32'}, {val: 1, type: 'i32'},
                             {val: 0,    type: 'i32'}, {val: 0, type: 'i32'}, {val: 0, type: 'i32'}], type: 'list' },
  'gnss-age':       { val: 0.0,  type: 'f32' },
};

let confValRefresh = null;
window.setConfVal = (key, val) => {
  if (window.configState) {
    if (window.configState[key]) window.configState[key].val = val;
    else window.configState[key] = { val, type: 'f64' };
  }
  if (confValRefresh) confValRefresh(key, val);
};

function createSimValueTab(label, stateObj, opts) {
  const existing = editorTabs.find(t => t.isSim && t.simLabel === label);
  if (existing) { switchEditorTab(existing.id); return existing._refresh; }
  opts = opts || {};
  const SIM_TYPES    = opts.types          || ['i', 'u', 'i32', 'u32', 'f32', 'f64', 'symbol', 'str', 'list'];
  const defaultType  = opts.defaultType    || 'f64';
  const keyLabel     = opts.keyLabel       || 'Key';
  const keyInputType = opts.keyType        || 'text';

  editorTabSeq++;
  const id = 'et' + editorTabSeq;
  const inputRefs = {}; // key -> { setVal, setType }

  const btn = document.createElement('button');
  btn.className = 'tab-btn';
  btn.dataset.tab = id;
  btn.addEventListener('click', () => switchEditorTab(id));
  const labelEl = document.createElement('span');
  labelEl.textContent = label;
  const closeEl = document.createElement('span');
  closeEl.className = 'tab-close';
  closeEl.textContent = '⊗';
  closeEl.addEventListener('click', e => { e.stopPropagation(); closeEditorTab(id); });
  btn.appendChild(labelEl);
  btn.appendChild(closeEl);
  document.getElementById('editor-tab-bar').insertBefore(btn, document.getElementById('btn-new-editor-tab'));

  const pane = document.createElement('div');
  pane.className = 'sim-pane sim-value-pane';

  const table = document.createElement('table');
  table.className = 'sim-value-table';
  const thead = document.createElement('thead');
  const hrow = document.createElement('tr');
  [keyLabel, 'Type', 'Value'].forEach(t => {
    const th = document.createElement('th'); th.textContent = t; hrow.appendChild(th);
  });
  thead.appendChild(hrow);
  table.appendChild(thead);
  const tbody = document.createElement('tbody');
  table.appendChild(tbody);
  pane.appendChild(table);

  function makeTypeSelect(entry) {
    const sel = document.createElement('select');
    sel.className = 'sim-type-select';
    SIM_TYPES.forEach(t => {
      const o = document.createElement('option');
      o.value = t; o.textContent = t;
      if (t === entry.type) o.selected = true;
      sel.appendChild(o);
    });
    return sel;
  }

  function makeValInput(entry) {
    const inp = document.createElement('input');
    inp.className = 'sim-value-input';
    if (entry.type === 'symbol' || entry.type === 'str') {
      inp.type = 'text';
      inp.value = entry.val || '';
      inp.addEventListener('change', () => { entry.val = inp.value; });
    } else {
      inp.type = 'number'; inp.step = 'any';
      inp.value = entry.val;
      inp.addEventListener('change', () => { entry.val = parseFloat(inp.value) || 0; });
    }
    return inp;
  }

  function makeSubRow(elem, idx, insertAfter) {
    const sr = document.createElement('tr');
    sr.className = 'sim-list-subrow';
    const tdIdx = document.createElement('td');
    tdIdx.className = 'sim-value-key sim-list-idx';
    tdIdx.textContent = '[' + idx + ']';
    const tdType = document.createElement('td');
    const sel = makeTypeSelect(elem);
    tdType.appendChild(sel);
    const tdVal = document.createElement('td');
    let inp = makeValInput(elem);
    tdVal.appendChild(inp);
    sel.addEventListener('change', () => {
      elem.type = sel.value;
      const n = makeValInput(elem);
      tdVal.replaceChild(n, inp);
      inp = n;
    });
    sr.appendChild(tdIdx); sr.appendChild(tdType); sr.appendChild(tdVal);
    insertAfter.insertAdjacentElement('afterend', sr);
    return sr;
  }

  function addRow(key, entry) {
    if (!entry || typeof entry !== 'object') entry = { val: entry, type: defaultType };
    if (entry.type === 'list' && !Array.isArray(entry.val)) entry.val = [];

    const tr = document.createElement('tr');
    let subRows = [];

    const tdKey = document.createElement('td');
    tdKey.className = 'sim-value-key';
    tdKey.textContent = key;

    const tdType = document.createElement('td');
    const typeSelect = makeTypeSelect(entry);
    tdType.appendChild(typeSelect);

    const tdVal = document.createElement('td');
    tr.appendChild(tdKey); tr.appendChild(tdType); tr.appendChild(tdVal);
    tbody.appendChild(tr);

    function renderVal() {
      while (tdVal.firstChild) tdVal.removeChild(tdVal.firstChild);
      subRows.forEach(sr => tbody.removeChild(sr));
      subRows = [];

      if (entry.type === 'list') {
        if (!Array.isArray(entry.val)) entry.val = [];
        const lenInp = document.createElement('input');
        lenInp.type = 'number'; lenInp.min = 0; lenInp.max = 64; lenInp.step = 1;
        lenInp.value = entry.val.length;
        lenInp.className = 'sim-list-len';
        lenInp.addEventListener('change', () => {
          const n = Math.max(0, Math.min(64, parseInt(lenInp.value) || 0));
          lenInp.value = n;
          while (entry.val.length < n) entry.val.push({ val: 0, type: SIM_TYPES[0] });
          while (entry.val.length > n) entry.val.pop();
          subRows.forEach(sr => tbody.removeChild(sr));
          subRows = [];
          let after = tr;
          entry.val.forEach((elem, i) => { const sr = makeSubRow(elem, i, after); after = sr; subRows.push(sr); });
        });
        tdVal.appendChild(lenInp);
        let after = tr;
        entry.val.forEach((elem, i) => { const sr = makeSubRow(elem, i, after); after = sr; subRows.push(sr); });
      } else {
        const inp = makeValInput(entry);
        tdVal.appendChild(inp);
        inputRefs[key] = {
          setVal:  (v) => { entry.val = v; inp.value = v; },
          setType: (t) => { if (entry.type === t) return; entry.type = t; typeSelect.value = t; renderVal(); }
        };
      }
    }

    typeSelect.addEventListener('change', () => {
      if (typeSelect.value === 'list' && !Array.isArray(entry.val)) entry.val = [];
      entry.type = typeSelect.value;
      renderVal();
    });

    renderVal();

    if (entry.type === 'list') {
      inputRefs[key] = { setVal: () => {}, setType: () => {} };
    }
  }

  Object.entries(stateObj).forEach(([k, e]) => addRow(k, e));

  const addArea = document.createElement('div');
  addArea.className = 'sim-value-add';
  const keyInp = document.createElement('input');
  keyInp.type = keyInputType; keyInp.placeholder = keyLabel.toLowerCase(); keyInp.className = 'sim-value-key-inp';
  const addTypeSelect = makeTypeSelect({ type: SIM_TYPES[0] });
  const valInp = document.createElement('input');
  valInp.type = 'number'; valInp.step = 'any'; valInp.placeholder = '0';
  valInp.className = 'sim-value-input';
  addTypeSelect.addEventListener('change', () => {
    const t = addTypeSelect.value;
    valInp.style.display = t === 'list' ? 'none' : '';
    valInp.type = (t === 'symbol' || t === 'str') ? 'text' : 'number';
  });
  const addBtn = document.createElement('button');
  addBtn.textContent = '+ Add';
  addBtn.addEventListener('click', () => {
    const raw = keyInputType === 'number' ? parseInt(keyInp.value) : keyInp.value.trim();
    if (raw === '' || raw === null || (keyInputType === 'number' && isNaN(raw))) return;
    const k = keyInputType === 'number' ? raw : raw;
    const t = addTypeSelect.value;
    const v = t === 'list' ? [] : (t === 'symbol' || t === 'str') ? (valInp.value || '') : (parseFloat(valInp.value) || 0);
    const entry = { val: v, type: t };
    stateObj[k] = entry;
    if (inputRefs[k]) { inputRefs[k].setVal(v); inputRefs[k].setType(t); }
    else addRow(k, entry);
    keyInp.value = ''; valInp.value = '';
  });
  addArea.appendChild(keyInp);
  addArea.appendChild(addTypeSelect);
  addArea.appendChild(valInp);
  addArea.appendChild(addBtn);
  pane.appendChild(addArea);

  document.getElementById('editor-tab-contents').appendChild(pane);

  const tab = { id, btn, pane, cm: null, labelEl, filename: null, baseUrl: null, isSim: true, simLabel: label };
  editorTabs.push(tab);
  switchEditorTab(id);

  const refresh = (key, val, type) => {
    if (!stateObj[key]) {
      stateObj[key] = { val, type: type || defaultType };
      addRow(key, stateObj[key]);
    } else {
      stateObj[key].val = val;
      if (type !== undefined) stateObj[key].type = type;
      if (inputRefs[key]) {
        inputRefs[key].setVal(val);
        if (type !== undefined) inputRefs[key].setType(type);
      }
    }
  };
  tab._refresh = refresh;
  return refresh;
}

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

const consoleHistory = document.createElement('span');
consoleHistory.style.cssText = 'white-space:pre;display:block;';
consolePane.appendChild(consoleHistory);

const consoleCurrentLine = document.createElement('div');
consoleCurrentLine.id = 'console-current-line';
const consolePromptSpan = document.createElement('span');
consolePromptSpan.id = 'console-prompt';
consolePromptSpan.textContent = '# ';
const consoleInputDisplay = document.createElement('span');
const consoleCursor = document.createElement('span');
consoleCursor.id = 'console-cursor';
consoleCursor.textContent = ' ';
consoleCurrentLine.appendChild(consolePromptSpan);
consoleCurrentLine.appendChild(consoleInputDisplay);
consoleCurrentLine.appendChild(consoleCursor);
consolePane.appendChild(consoleCurrentLine);

const consoleInput = document.createElement('textarea');
consoleInput.autocomplete = 'off';
consoleInput.disabled = true;
consoleInput.style.cssText = 'position:fixed;opacity:0;pointer-events:none;width:1px;height:1px;top:-1px;left:-1px;resize:none;';
document.body.appendChild(consoleInput);

consolePane.addEventListener('click', () => consoleInput.focus());
consoleInput.addEventListener('input', () => {
  consoleInputDisplay.textContent = consoleInput.value;
});

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
    inputStyle: 'contenteditable',
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
      'F1': cm => {
        const range = cm.findWordAt(cm.getCursor());
        const word  = cm.getRange(range.anchor, range.head);
        switchTab('docs');
        docsSearchInput.value = word;
        docsSearch(word);
      },
    }
  });
  cm.setSize('100%', '100%');
  cm.on('paste', () => setTimeout(() => cm.focus(), 20));

  const tab = { id, btn, pane, cm, labelEl, filename: null, baseUrl: null };
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
      if (t.cm) t.cm.refresh();
    }
  });
}

function closeEditorTab(id) {
  const tab = editorTabs.find(t => t.id === id);
  if (!tab) return;
  if (!tab.isSim && editorTabs.filter(t => !t.isSim).length <= 1) return;
  const idx = editorTabs.findIndex(t => t.id === id);
  if (idx < 0) return;
  tab.btn.remove();
  tab.pane.remove();
  editorTabs.splice(idx, 1);
  if (activeEditor && activeEditor.id === id) {
    switchEditorTab(editorTabs[Math.max(0, idx - 1)].id);
  }
}

function tabMatchesFilename(t, filename) {
  return t.filename === filename || t.labelEl.textContent === filename;
}

// RTS tab (permanent)
const rtsTabBtn = document.createElement('button');
rtsTabBtn.className = 'tab-btn';
rtsTabBtn.dataset.tab = 'rts';
rtsTabBtn.addEventListener('click', () => { switchTab('rts'); if (typeof refreshFsBrowser === 'function') refreshFsBrowser(); });
const rtsLabelEl = document.createElement('span');
rtsLabelEl.textContent = 'RTS';
rtsTabBtn.appendChild(rtsLabelEl);
document.getElementById('output-tab-bar').appendChild(rtsTabBtn);

const rtsPane = document.createElement('div');
rtsPane.id        = 'output-tab-rts';
rtsPane.className = 'tab-pane';
rtsPane.style.cssText = 'padding:10px;overflow:auto;';
document.getElementById('output-tab-contents').appendChild(rtsPane);

const rtsLiveDiv = document.createElement('div');
rtsPane.appendChild(rtsLiveDiv);

const rtsFsDiv = document.createElement('div');
rtsPane.appendChild(rtsFsDiv);

// Docs tab (permanent)
const docsTabBtn = document.createElement('button');
docsTabBtn.className = 'tab-btn';
docsTabBtn.dataset.tab = 'docs';
docsTabBtn.addEventListener('click', () => switchTab('docs'));
const docsLabelEl = document.createElement('span');
docsLabelEl.textContent = 'Docs';
docsTabBtn.appendChild(docsLabelEl);
document.getElementById('output-tab-bar').appendChild(docsTabBtn);

const docsPane = document.createElement('div');
docsPane.id = 'output-tab-docs';
docsPane.className = 'tab-pane';
document.getElementById('output-tab-contents').appendChild(docsPane);

const docsSearchInput = document.createElement('input');
docsSearchInput.type = 'text';
docsSearchInput.id = 'docs-search';
docsSearchInput.placeholder = 'Search docs... (F1 in editor)';
docsPane.appendChild(docsSearchInput);

const docsResults = document.createElement('div');
docsResults.id = 'docs-results';
docsPane.appendChild(docsResults);

// About tab (permanent)
const aboutTabBtn = document.createElement('button');
aboutTabBtn.className = 'tab-btn';
aboutTabBtn.dataset.tab = 'about';
aboutTabBtn.addEventListener('click', () => switchTab('about'));
const aboutLabelEl = document.createElement('span');
aboutLabelEl.textContent = 'About';
aboutTabBtn.appendChild(aboutLabelEl);
document.getElementById('output-tab-bar').appendChild(aboutTabBtn);

const aboutPane = document.createElement('div');
aboutPane.id = 'output-tab-about';
aboutPane.className = 'tab-pane';
aboutPane.innerHTML = `
<h2>LispBM WebAssembly REPL</h2>
<p>
  LispBM is an embeddable Lisp/Scheme for microcontrollers, running here via WebAssembly.
  Write code in the editor tabs on the right, then press <code>Load</code> to evaluate the whole buffer,
  or type expressions directly into the console.
  Use <code>Shift+Enter</code> in the console for multi-line input.
</p>
<p>
  Files live in an in-memory filesystem (MEMFS). Use the <strong>RTS</strong> tab to browse, upload,
  and download files. The <code>/libs</code> directory is pre-populated at startup.
  Import from MEMFS with absolute paths: <code>(import "/libs/dsp_lang.lisp" 'dsp)</code>.
</p>

<h2>Time</h2>
<table class="about-table">
  <tr><th>Extension</th><th>Signature</th><th>Description</th></tr>
  <tr><td>systime</td><td>(systime)</td><td>Current time as u32 milliseconds</td></tr>
  <tr><td>secs-since</td><td>(secs-since t0)</td><td>Seconds elapsed since t0 as float</td></tr>
</table>

<h2>Output</h2>
<table class="about-table">
  <tr><th>Extension</th><th>Signature</th><th>Description</th></tr>
  <tr><td>print</td><td>(print val ...)</td><td>Print values to console with newline</td></tr>
</table>

<h2>Plotting &amp; Canvas</h2>
<table class="about-table">
  <tr><th>Extension</th><th>Signature</th><th>Description</th></tr>
  <tr><td>wasm-plot</td><td>(wasm-plot buf "title")</td><td>Plot tab from a float32 byte array</td></tr>
  <tr><td>wasm-plot-multi</td><td>(wasm-plot-multi '(buf ...) "title")</td><td>Multi-series plot from a list of float32 arrays</td></tr>
  <tr><td>wasm-plot-xy</td><td>(wasm-plot-xy xbuf ybuf "title")</td><td>XY plot from two float32 byte arrays</td></tr>
  <tr><td>wasm-create-canvas</td><td>(wasm-create-canvas w h ["title"])</td><td>Canvas tab for display library drawing</td></tr>
</table>

<h2>Import</h2>
<table class="about-table">
  <tr><th>Extension</th><th>Signature</th><th>Description</th></tr>
  <tr><td>import</td><td>(import "/path/or/url" 'sym)</td><td>Load a file into sym: absolute URL &rarr; relative URL (if base set) &rarr; editor tab &rarr; MEMFS</td></tr>
</table>

<h2>File I/O (MEMFS)</h2>
<table class="about-table">
  <tr><th>Extension</th><th>Signature</th><th>Description</th></tr>
  <tr><td>fopen</td><td>(fopen "path" "mode")</td><td>Open a MEMFS file, returns file handle or nil</td></tr>
  <tr><td>fclose</td><td>(fclose fh)</td><td>Close a file handle</td></tr>
  <tr><td>load-file</td><td>(load-file fh)</td><td>Read entire file into a byte array</td></tr>
  <tr><td>fread</td><td>(fread fh buf)</td><td>Read into a byte array, returns bytes read</td></tr>
  <tr><td>fread-byte</td><td>(fread-byte fh)</td><td>Read one byte as char, nil at EOF</td></tr>
  <tr><td>fwrite</td><td>(fwrite fh buf)</td><td>Write a byte array (full contents, including nulls)</td></tr>
  <tr><td>fwrite-str</td><td>(fwrite-str fh str)</td><td>Write a string up to its null terminator</td></tr>
  <tr><td>fwrite-value</td><td>(fwrite-value fh val)</td><td>Write a flattened LispBM value</td></tr>
  <tr><td>fseek</td><td>(fseek fh offset 'seek-set|'seek-cur|'seek-end)</td><td>Seek within a file</td></tr>
  <tr><td>ftell</td><td>(ftell fh)</td><td>Current file position as i64</td></tr>
  <tr><td>flist</td><td>(flist ["path"])</td><td>List of filenames in a directory (default: /)</td></tr>
  <tr><td>wasm-save-file</td><td>(wasm-save-file "path" ["download-name"])</td><td>Download a MEMFS file to the user's disk</td></tr>
</table>

<h2>Filesystem Operations</h2>
<table class="about-table">
  <tr><th>Extension</th><th>Signature</th><th>Description</th></tr>
  <tr><td>fs-pwd</td><td>(fs-pwd)</td><td>Current working directory as a string</td></tr>
  <tr><td>fs-cd</td><td>(fs-cd "path")</td><td>Change directory, returns t or nil</td></tr>
  <tr><td>fs-mkdir</td><td>(fs-mkdir "path")</td><td>Create a directory, returns t or nil</td></tr>
  <tr><td>fs-rm</td><td>(fs-rm "path")</td><td>Remove a file, returns t or nil</td></tr>
  <tr><td>fs-mv</td><td>(fs-mv "src" "dst")</td><td>Rename or move a file, returns t or nil</td></tr>
  <tr><td>fs-exists</td><td>(fs-exists "path")</td><td>t if path exists, else nil</td></tr>
  <tr><td>fs-stat</td><td>(fs-stat "path")</td><td>(size is-dir) tuple, or nil if not found</td></tr>
  <tr><td>fs-ls</td><td>(fs-ls ["path"])</td><td>List of (name size is-dir) tuples (default: cwd)</td></tr>
  <tr><td>fs-open</td><td>(fs-open "path")</td><td>Open a MEMFS file in a new editor tab</td></tr>
</table>

<p style="color:#555;font-size:11px;">Symbols for fseek: <code>'seek-set</code> &nbsp; <code>'seek-cur</code> &nbsp; <code>'seek-end</code></p>
`;
document.getElementById('output-tab-contents').appendChild(aboutPane);

let pagefind = null;
async function initPagefind() {
  if (pagefind) return pagefind;
  pagefind = await import('/lispbm-reference-manual/html/pagefind/pagefind.js');
  return pagefind;
}

async function docsSearch(query) {
  if (!query.trim()) { docsResults.innerHTML = ''; return; }
  docsResults.innerHTML = '<div style="color:#666;font-size:12px;padding:8px;">Searching...</div>';
  try {
    const pf = await initPagefind();
    const result = await pf.search(query);
    const data = await Promise.all(result.results.slice(0, 10).map(r => r.data()));
    docsResults.innerHTML = '';
    if (!data.length) {
      docsResults.innerHTML = '<div style="color:#666;font-size:12px;padding:8px;">No results.</div>';
      return;
    }
    data.forEach(r => {
      const item = document.createElement('div');
      item.className = 'docs-result';
      item.innerHTML = '<div class="docs-result-title">' + (r.meta.title || r.url) + '</div>' +
                       '<div class="docs-result-excerpt">' + r.excerpt + '</div>';
      item.addEventListener('click', () => openDocPage(r.url));
      docsResults.appendChild(item);
    });
  } catch(e) {
    docsResults.innerHTML = '<div style="color:#e06c75;font-size:12px;padding:8px;">Search failed: ' + e.message + '</div>';
  }
}

docsSearchInput.addEventListener('input', () => docsSearch(docsSearchInput.value));

let docsIframe = null;
let docsBackBtn = null;

function openDocPage(url) {
  docsSearchInput.style.display = 'none';
  docsResults.style.display = 'none';

  if (!docsBackBtn) {
    docsBackBtn = document.createElement('button');
    docsBackBtn.textContent = '\u2190 Back to results';
    docsBackBtn.style.cssText = 'align-self:flex-start;background:#3a3a3a;border:1px solid #555;color:#d4d4d4;font-size:12px;padding:3px 10px;flex-shrink:0;';
    docsBackBtn.addEventListener('click', () => {
      docsIframe.style.display = 'none';
      docsBackBtn.style.display = 'none';
      docsSearchInput.style.display = '';
      docsResults.style.display = '';
    });
    docsPane.appendChild(docsBackBtn);
  } else {
    docsBackBtn.style.display = '';
  }

  if (!docsIframe) {
    docsIframe = document.createElement('iframe');
    docsIframe.style.cssText = 'flex:1;border:none;background:#fff;';
    docsPane.appendChild(docsIframe);
  } else {
    docsIframe.style.display = '';
  }

  docsIframe.src = url;
}

// ------------------------------------------------------------
// Canvas tabs
// ------------------------------------------------------------
let canvasTabSeq = 0;
const canvasTabs = {};

window.createCanvasTab = function(w, h, title) {
  canvasTabSeq++;
  const cid   = canvasTabSeq;
  const tabId = 'canvas-' + cid;
  const label = (title && title.length) ? title : ('Canvas ' + cid);

  const btn = document.createElement('button');
  btn.className   = 'tab-btn';
  btn.dataset.tab = tabId;
  btn.addEventListener('click', () => switchTab(tabId));
  const labelEl = document.createElement('span');
  labelEl.textContent = label;
  const closeEl = document.createElement('span');
  closeEl.className   = 'tab-close';
  closeEl.textContent = '\u2297';
  closeEl.addEventListener('click', e => { e.stopPropagation(); closeTab(tabId); delete canvasTabs[cid]; });
  btn.appendChild(labelEl);
  btn.appendChild(closeEl);
  document.getElementById('output-tab-bar').appendChild(btn);

  const pane = document.createElement('div');
  pane.id        = 'output-tab-' + tabId;
  pane.className = 'tab-pane';
  pane.style.cssText = 'padding:8px;overflow:auto;background:#111;';
  document.getElementById('output-tab-contents').appendChild(pane);

  const toolbar = document.createElement('div');
  toolbar.style.cssText = 'display:flex;gap:6px;padding:0 0 4px 0;align-items:center;';

  const saveBtn = document.createElement('button');
  saveBtn.textContent = 'Save PNG';
  saveBtn.addEventListener('click', () => {
    const a = document.createElement('a');
    a.href     = canvas.toDataURL('image/png');
    a.download = label + '.png';
    a.click();
  });

  const scaleLabel = document.createElement('label');
  scaleLabel.textContent = 'Scale:';
  scaleLabel.style.cssText = 'font-size:12px;color:#888;';

  const scaleSelect = document.createElement('select');
  scaleSelect.style.cssText = 'background:#2d2d2d;color:#d4d4d4;border:1px solid #555;font-size:12px;';
  [1, 2, 3, 4, 5].forEach(n => {
    const opt = document.createElement('option');
    opt.value = n;
    opt.textContent = n + 'x';
    if (n === 1) opt.selected = true;
    scaleSelect.appendChild(opt);
  });
  scaleSelect.addEventListener('change', () => {
    const s = parseInt(scaleSelect.value);
    canvas.style.transformOrigin = 'top left';
    canvas.style.transform = s === 1 ? '' : `scale(${s})`;
    pane.style.overflow = 'auto';
  });

  toolbar.appendChild(saveBtn);
  toolbar.appendChild(scaleLabel);
  toolbar.appendChild(scaleSelect);
  pane.appendChild(toolbar);

  const canvas = document.createElement('canvas');
  canvas.width  = w;
  canvas.height = h;
  canvas.style.cssText = 'display:block;background:#000;image-rendering:pixelated;';
  pane.appendChild(canvas);
  const ctx = canvas.getContext('2d');

  canvasTabs[cid] = { canvas, ctx, tabId };
  switchTab(tabId);
  return cid;
};

window.canvasClear = function(canvasId, color) {
  const tab = canvasTabs[canvasId];
  if (!tab) return;
  const r = (color >>> 16) & 0xFF;
  const g = (color >>>  8) & 0xFF;
  const b =  color         & 0xFF;
  tab.ctx.fillStyle = `rgb(${r},${g},${b})`;
  tab.ctx.fillRect(0, 0, tab.canvas.width, tab.canvas.height);
};

window.countEditorTabMatches = function(filename) {
  return editorTabs.filter(t => tabMatchesFilename(t, filename)).length;
};

window.getEditorTabContent = function(filename) {
  const tab = editorTabs.find(t => !t.isSim && tabMatchesFilename(t, filename));
  return tab ? tab.cm.getValue() : null;
};

window.openFileInTab = function(filename, content) {
  const tab = createEditorTab(filename);
  tab.cm.setValue(content);
  tab.filename = filename;
};

document.getElementById('btn-new-editor-tab').addEventListener('click', () => {
  const n = prompt('Tab name:', 'untitled');
  if (n !== null) createEditorTab(n.trim() || 'untitled');
});

createEditorTab('untitled');

const fileInput = document.getElementById('file-input');
document.getElementById('btn-import').addEventListener('click', () => fileInput.click());

document.getElementById('btn-export').addEventListener('click', () => {
  if (!activeEditor || activeEditor.isSim) return;
  const filename = activeEditor.filename || 'untitled.lisp';
  const blob = new Blob([activeEditor.cm.getValue()], { type: 'text/plain' });
  const url  = URL.createObjectURL(blob);
  const a    = document.createElement('a');
  a.href = url; a.download = filename; a.click();
  URL.revokeObjectURL(url);
});

document.getElementById('btn-open').addEventListener('click', () => {
  if (typeof window.openFsDialog !== 'function') { fileInput.click(); return; }
  window.openFsDialog({ mode: 'open', title: 'Open from MEMFS' }).then(path => {
    if (!path) return;
    try {
      const name    = path.split('/').pop();
      const content = lbm.FS.readFile(path, { encoding: 'utf8' });
      const tab = createEditorTab(name);
      tab.cm.setValue(content);
      tab.filename = name;
    } catch(e) { appendOutput('Error opening ' + path + ': ' + e.message + '\n'); }
  });
});
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

document.getElementById('btn-open-url').addEventListener('click', () => {
  const url = prompt('Open URL:');
  if (!url || !url.trim()) return;
  fetch(url.trim())
    .then(r => { if (!r.ok) throw new Error('HTTP ' + r.status); return r.text(); })
    .then(code => {
      const name = url.trim().split('/').pop() || 'untitled';
      const tab = createEditorTab(name);
      tab.cm.setValue(code);
      tab.filename = name;
      tab.baseUrl  = url.trim();
    })
    .catch(e => alert('Failed to open URL: ' + e.message));
});

const simMenuBtn  = document.getElementById('sim-menu-btn');
const simDropdown = document.getElementById('sim-dropdown');
simMenuBtn.addEventListener('click', e => { e.stopPropagation(); simDropdown.classList.toggle('open'); });
document.addEventListener('click', () => simDropdown.classList.remove('open'));
document.querySelectorAll('.sim-dropdown-item').forEach(item => {
  item.addEventListener('click', () => {
    const type = item.dataset.sim;
    if (type === 'bms') {
      createSimValueTab('BMS', window.bmsState);
    } else if (type === 'config') {
      confValRefresh = createSimValueTab('Config', window.configState);
    } else if (type === 'eeprom') {
      eepromRefresh = createSimValueTab('EEPROM', window.eepromState,
        { types: ['i32', 'f32'], defaultType: 'i32', keyLabel: 'Addr', keyType: 'number' });
    } else if (type === 'gnss') {
      createSimValueTab('GNSS', window.gnssState);
    } else if (type === 'gpio') {
      createInspectTab();
    }
    simDropdown.classList.remove('open');
  });
});


const busyLed    = document.getElementById('busy-led');
const statusText = document.getElementById('status-text');
const stepsSelect = document.getElementById('steps-select');

const examplesModal = document.getElementById('examples-modal');
const examplesList  = document.getElementById('examples-list');

document.getElementById('btn-examples').addEventListener('click', () => {
  examplesList.innerHTML = '';
  fetch('examples/index.json?v=' + Date.now())
    .then(r => { if (!r.ok) throw new Error('HTTP ' + r.status); return r.json(); })
    .then(examples => {
      examples.forEach(ex => {
        const item = document.createElement('div');
        item.className = 'example-item';
        item.innerHTML = '<div class="ex-name">' + ex.name + '</div>' +
                         '<div class="ex-desc">' + (ex.description || '') + '</div>';
        item.addEventListener('click', () => {
          fetch('examples/' + ex.file)
            .then(r => r.text())
            .then(code => {
              const tab = createEditorTab(ex.name);
              tab.cm.setValue(code);
              examplesModal.classList.remove('open');
            });
        });
        examplesList.appendChild(item);
      });
      examplesModal.classList.add('open');
    })
    .catch(e => { alert('Failed to load examples: ' + e.message); });
});

document.getElementById('examples-close').addEventListener('click', () => {
  examplesModal.classList.remove('open');
});

examplesModal.addEventListener('click', e => {
  if (e.target === examplesModal) examplesModal.classList.remove('open');
});


const DARK_AXES = [{ stroke: '#666', grid: { stroke: '#222' }, ticks: { stroke: '#222' } },
                   { stroke: '#666', grid: { stroke: '#222' }, ticks: { stroke: '#222' } },];

function mkPlotTab(title) {
    
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
    const h    = Math.max(rect.height - 48, 200);

    return {id, label, pane, w, h};
}

// lbm variable in the lambda will be bound to the WASM
// module (LispBM repl compiled into wasm) and the lbm
// value is then a handle through which all interaction with
// lispbm runtime happens.
LispBM().then(lbm => {
  const btnLoad = document.getElementById('btn-load');
  const status  = document.getElementById('status');

  function appendOutput(text) {
    consoleHistory.textContent += text;
    consolePane.scrollTop = consolePane.scrollHeight;
  }

  function pollOutput() {
    const text = lbm.ccall('lbm_wasm_get_output', 'string', [], []);
    if (text && text.length > 0) {
      appendOutput(text);
      lbm.ccall('lbm_wasm_clear_output', null, [], []);
    }
  }

  const wheelZoomPlugin = {
    hooks: {
      ready(u) {
        const over = u.over;
        over.addEventListener('wheel', e => {
          e.preventDefault();
          const factor = e.deltaY < 0 ? 0.75 : 1.33;
          const left   = u.cursor.left;
          const xMin   = u.scales.x.min, xMax = u.scales.x.max;
          const range  = (xMax - xMin) * factor;
          const mid    = u.posToVal(left, 'x');
          u.setScale('x', { min: mid - range / 2, max: mid + range / 2 });
        });

        let panning = false, dragStartX, scaleMin, scaleMax;
        window.addEventListener('keydown', e => {
          if (e.key === 'Shift') u.cursor.drag.x = false;
        });
        window.addEventListener('keyup', e => {
          if (e.key === 'Shift') u.cursor.drag.x = true;
        });
        over.addEventListener('mousedown', e => {
          if (!e.shiftKey) return;
          e.preventDefault();
          panning    = true;
          dragStartX = e.clientX;
          scaleMin   = u.scales.x.min;
          scaleMax   = u.scales.x.max;
        });
        window.addEventListener('mousemove', e => {
          if (!panning) return;
          const dx    = dragStartX - e.clientX;
          const range = scaleMax - scaleMin;
          const shift = (dx / u.width) * range;
          u.setScale('x', { min: scaleMin + shift, max: scaleMax + shift });
        });
        window.addEventListener('mouseup', () => { panning = false; });
      }
    }
  };

  function addPlotToolbar(pane, label, getDataFn) {
    const toolbar = document.createElement('div');
    toolbar.style.cssText = 'display:flex;gap:6px;padding:4px 8px;';

    const pngBtn = document.createElement('button');
    pngBtn.textContent = 'Save PNG';
    pngBtn.addEventListener('click', () => {
      const canvas = pane.querySelector('canvas');
      if (!canvas) return;
      const a = document.createElement('a');
      a.href     = canvas.toDataURL('image/png');
      a.download = label + '.png';
      a.click();
    });

    const csvBtn = document.createElement('button');
    csvBtn.textContent = 'Save CSV';
    csvBtn.addEventListener('click', () => {
      const { xs, yArrays } = getDataFn();
      const headers = ['x', ...yArrays.map((_, i) => 'y' + (yArrays.length > 1 ? i : ''))].join(',');
      const rows    = xs.map((x, i) => [x, ...yArrays.map(y => y[i] ?? '')].join(','));
      const blob    = new Blob([headers + '\n' + rows.join('\n')], { type: 'text/csv' });
      const a       = document.createElement('a');
      a.href        = URL.createObjectURL(blob);
      a.download    = label + '.csv';
      a.click();
      URL.revokeObjectURL(a.href);
    });

    toolbar.appendChild(pngBtn);
    toolbar.appendChild(csvBtn);
    pane.appendChild(toolbar);
  }

  window.createPlotTab = function(buf, nbytes, title) {
    //const ptr    = lbm.ccall('lbm_wasm_buf_ptr', 'number', ['number'], [slot]);
    const nFloat = (nbytes / 4) | 0;

    const floats = new Float32Array(lbm.HEAP8.buffer, buf, nFloat);
    const ys     = Array.from(floats);
    const xs     = Array.from({length: ys.length}, (_, i) => i);

    const {id, label, pane, w, h} =  mkPlotTab(title)

    addPlotToolbar(pane, label, () => ({ xs, yArrays: [ys] }));
    new uPlot({
      title:  label,
      width:  w,
      height: h,
      series: [
        {},
        { label: 'value', stroke: '#4ec9b0', width: 2, fill: 'rgba(78,201,176,0.08)' }
      ],
      axes: DARK_AXES,
      scales: { x: { time: false } },
      cursor: { stroke: '#569cd6', width: 1 },
      plugins: [wheelZoomPlugin],
    }, [xs, ys], pane);
  };

  window.createXYPlotTab = function(xbuf, xbytes, ybuf, ybytes, title) {
    const xs = Array.from(new Float32Array(lbm.HEAP8.buffer, xbuf, (xbytes / 4) | 0));
    const ys = Array.from(new Float32Array(lbm.HEAP8.buffer, ybuf, (ybytes / 4) | 0));

    const {id, label, pane, w, h} =  mkPlotTab(title)

    addPlotToolbar(pane, label, () => ({ xs, yArrays: [ys] }));

    new uPlot({
      title:  label,
      width:  w,
      height: h,
      series: [
        {},
        { label: 'y', stroke: '#4ec9b0', width: 2 }
      ],
      axes: DARK_AXES,
      scales: { x: { time: false } },
      cursor: { stroke: '#569cd6', width: 1 },
      plugins: [wheelZoomPlugin],
    }, [xs, ys], pane);
  };

  function refreshRTS() {
    rtsLiveDiv.innerHTML = '';

    // --- Stats ---
    const statsJson = lbm.ccall('lbm_wasm_get_stats', 'string', [], []);
    let st;
    try { st = JSON.parse(statsJson); } catch(e) { st = null; }

    if (st) {
      const heapUsed = st.heap_size - st.heap_free;
      const memUsed  = st.mem_size  - st.mem_free;

      const grid = document.createElement('div');
      grid.style.cssText = 'display:grid;grid-template-columns:1fr 1fr;gap:2px 24px;font-size:12px;margin-bottom:12px;';

      const rows = [
        ['Heap used',          `${heapUsed} / ${st.heap_size} cells`],
        ['LBM memory used',    `${memUsed.toLocaleString()} / ${st.mem_size.toLocaleString()} bytes`],
        ['LBM memory free',    `${st.mem_free.toLocaleString()} bytes (longest block: ${st.mem_longest_free.toLocaleString()})`],
        ['LBM memory peak',    `${st.mem_max_used_pct.toFixed(1)}%`],
        ['Allocated arrays',   `${st.num_alloc_arrays}`],
        ['GC runs',            `${st.gc_num}`],
        ['GC recovered',       `${st.gc_recovered} cells, ${st.gc_recovered_arrays} arrays`],
        ['GC marked',          `${st.gc_marked}`],
        ['GC stack',           `max ${st.gc_stack_max} / ${st.gc_stack_size}`],
      ];

      rows.forEach(([label, value]) => {
        const lEl = document.createElement('div');
        lEl.textContent = label;
        lEl.style.cssText = 'color:#569cd6;';
        const vEl = document.createElement('div');
        vEl.textContent = value;
        vEl.style.color = '#d4d4d4';
        grid.appendChild(lEl);
        grid.appendChild(vEl);
      });

      rtsLiveDiv.appendChild(grid);

      const div = document.createElement('div');
      div.style.cssText = 'border-top:1px solid #333;margin-bottom:10px;';
      rtsLiveDiv.appendChild(div);
    }

    // --- Thread list ---
    const json = lbm.ccall('lbm_wasm_get_ctxs', 'string', [], []);
    let ctxs;
    try { ctxs = JSON.parse(json); } catch(e) { return; }

    if (ctxs.length === 0) {
      const msg = document.createElement('div');
      msg.style.cssText = 'color:#666;font-size:12px;';
      msg.textContent = 'No running threads.';
      rtsLiveDiv.appendChild(msg);
    }

    if (ctxs.length > 0) {
    const table = document.createElement('table');
    table.style.cssText = 'width:100%;border-collapse:collapse;font-size:12px;';

    const hrow = document.createElement('tr');
    ['CID', 'Name', 'State', ''].forEach(h => {
      const th = document.createElement('th');
      th.textContent = h;
      th.style.cssText = 'text-align:left;color:#569cd6;padding:4px 8px;border-bottom:1px solid #333;';
      hrow.appendChild(th);
    });
    table.appendChild(hrow);

    ctxs.forEach(ctx => {
      const tr = document.createElement('tr');
      [ctx.cid, ctx.name || '\u2014', ctx.state].forEach(v => {
        const td = document.createElement('td');
        td.textContent = v;
        td.style.cssText = 'padding:4px 8px;border-bottom:1px solid #222;color:#d4d4d4;';
        tr.appendChild(td);
      });
      const tdBtn = document.createElement('td');
      tdBtn.style.cssText = 'padding:4px 8px;border-bottom:1px solid #222;';
      const killBtn = document.createElement('button');
      killBtn.textContent = 'Kill';
      killBtn.style.cssText = 'background:#6b1010;padding:2px 10px;font-size:11px;';
      killBtn.addEventListener('click', () => {
        lbm.ccall('lbm_wasm_eval', null, ['string'], ['(kill ' + ctx.cid + ' nil)']);
      });
      tdBtn.appendChild(killBtn);
      tr.appendChild(tdBtn);
      table.appendChild(tr);
    });

    rtsLiveDiv.appendChild(table);
    } // end ctxs.length > 0
  }

  // ------------------------------------------------------------
  // MEMFS file dialog
  // openFsDialog({ mode: 'open'|'save', title, filename }) -> Promise<string|null>
  // Resolves to an absolute MEMFS path or null if cancelled.
  // ------------------------------------------------------------
  const fsDialog       = document.getElementById('fs-dialog');
  const fsDialogTitle  = document.getElementById('fs-dialog-title');
  const fsDialogPath   = document.getElementById('fs-dialog-path');
  const fsDialogList   = document.getElementById('fs-dialog-list');
  const fsDialogFile   = document.getElementById('fs-dialog-filename');
  const fsDialogOk      = document.getElementById('fs-dialog-ok');
  const fsDialogCancel  = document.getElementById('fs-dialog-cancel');
  const fsDialogMkdir   = document.getElementById('fs-dialog-mkdir-btn');
  const fsDialogMkdirIn = document.getElementById('fs-dialog-mkdir-inp');

  let fsDialogResolve = null;
  let fsDialogCwd     = '/';

  function fsDialogNavigate(path) {
    fsDialogCwd = path;
    fsDialogPath.textContent = path;
    fsDialogList.innerHTML = '';

    if (path !== '/') {
      const upEl = document.createElement('div');
      upEl.className = 'fs-dialog-entry fs-dialog-dir';
      upEl.innerHTML = '<span class="fs-dialog-entry-name">↑ ..</span>';
      upEl.addEventListener('click', () => {
        const parent = path.substring(0, path.lastIndexOf('/')) || '/';
        fsDialogNavigate(parent);
      });
      fsDialogList.appendChild(upEl);
    }

    let entries = [];
    try { entries = lbm.FS.readdir(path).filter(e => e !== '.' && e !== '..'); } catch(e) {}
    entries.sort((a, b) => {
      const aDir = fsDialogIsDir(fsDialogJoin(path, a));
      const bDir = fsDialogIsDir(fsDialogJoin(path, b));
      if (aDir !== bDir) return aDir ? -1 : 1;
      return a.localeCompare(b);
    });
    entries.forEach(name => {
      const full  = fsDialogJoin(path, name);
      const isDir = fsDialogIsDir(full);
      const el    = document.createElement('div');
      el.className = 'fs-dialog-entry ' + (isDir ? 'fs-dialog-dir' : 'fs-dialog-file');
      el.innerHTML = '<span class="fs-dialog-entry-name">' + (isDir ? '▶ ' : '  ') + name + '</span>';
      el.addEventListener('click', () => {
        if (isDir) {
          fsDialogNavigate(full);
        } else {
          fsDialogList.querySelectorAll('.fs-dialog-entry').forEach(e => e.classList.remove('selected'));
          el.classList.add('selected');
          fsDialogFile.value = name;
          if (fsDialogOk.textContent === 'Open') fsDialogCommit();
        }
      });
      el.addEventListener('dblclick', () => {
        if (!isDir) { fsDialogFile.value = name; fsDialogCommit(); }
      });
      fsDialogList.appendChild(el);
    });
  }

  function fsDialogJoin(dir, name) {
    return (dir === '/' ? '' : dir) + '/' + name;
  }

  function fsDialogIsDir(path) {
    try { return lbm.FS.isDir(lbm.FS.stat(path).mode); } catch(e) { return false; }
  }

  function fsDialogCommit() {
    const name = fsDialogFile.value.trim();
    if (!name) return;
    const full = fsDialogJoin(fsDialogCwd, name);
    fsDialog.classList.remove('open');
    const resolve = fsDialogResolve;
    fsDialogResolve = null;
    if (resolve) resolve(full);
  }

  function fsDialogMkdirCommit() {
    const name = fsDialogMkdirIn.value.trim();
    if (!name) return;
    const full = fsDialogJoin(fsDialogCwd, name);
    try { lbm.FS.mkdir(full); } catch(e) {}
    fsDialogMkdirIn.value = '';
    fsDialogNavigate(full);
  }
  fsDialogMkdir.addEventListener('click', fsDialogMkdirCommit);
  fsDialogMkdirIn.addEventListener('keydown', e => { if (e.key === 'Enter') fsDialogMkdirCommit(); });

  fsDialogOk.addEventListener('click', fsDialogCommit);
  fsDialogCancel.addEventListener('click', () => {
    fsDialog.classList.remove('open');
    const resolve = fsDialogResolve;
    fsDialogResolve = null;
    if (resolve) resolve(null);
  });
  fsDialog.addEventListener('click', e => {
    if (e.target === fsDialog) fsDialogCancel.click();
  });
  fsDialogFile.addEventListener('keydown', e => { if (e.key === 'Enter') fsDialogCommit(); });

  window.openFsDialog = function({ mode = 'open', title, filename = '' } = {}) {
    return new Promise(resolve => {
      fsDialogResolve = resolve;
      fsDialogTitle.textContent = title || (mode === 'save' ? 'Save to MEMFS' : 'Open from MEMFS');
      fsDialogFile.value = filename;
      const filenameRow = document.getElementById('fs-dialog-filename-row');
      const mkdirRow    = document.getElementById('fs-dialog-mkdir-row');
      filenameRow.style.display = mode === 'save' ? 'flex'  : 'none';
      mkdirRow.style.display    = mode === 'save' ? 'flex'  : 'none';
      fsDialogOk.textContent = mode === 'save' ? 'Save' : 'Open';
      fsDialogNavigate(fsDialogCwd);
      fsDialog.classList.add('open');
      if (mode === 'save') setTimeout(() => fsDialogFile.focus(), 50);
    });
  };

  function memfsZipDir(dirPath) {
    const files = {};
    function walk(path, prefix) {
      let entries = [];
      try { entries = lbm.FS.readdir(path).filter(e => e !== '.' && e !== '..'); } catch(e) {}
      entries.forEach(name => {
        const full = (path === '/' ? '' : path) + '/' + name;
        const rel  = prefix ? prefix + '/' + name : name;
        let isDir = false;
        try { isDir = lbm.FS.isDir(lbm.FS.stat(full).mode); } catch(e) {}
        if (isDir) walk(full, rel);
        else files[rel] = lbm.FS.readFile(full);
      });
    }
    walk(dirPath, '');
    return fflate.zipSync(files);
  }

  function memfsUnzipInto(zipData, destDir) {
    const unzipped = fflate.unzipSync(new Uint8Array(zipData));
    Object.entries(unzipped).forEach(([relPath, data]) => {
      const parts = relPath.split('/').filter(Boolean);
      if (!parts.length) return;
      let cur = destDir === '/' ? '' : destDir;
      parts.slice(0, -1).forEach(part => {
        cur = cur + '/' + part;
        try { lbm.FS.mkdir(cur); } catch(e) {}
      });
      const filename = parts[parts.length - 1];
      lbm.FS.writeFile(cur + '/' + filename, data);
    });
  }

  function refreshFsBrowser() {
    rtsFsDiv.innerHTML = '';
    const fsSep = document.createElement('div');
    fsSep.style.cssText = 'border-top:1px solid #333;margin:10px 0;';
    rtsFsDiv.appendChild(fsSep);

    const fsHeader = document.createElement('div');
    fsHeader.style.cssText = 'display:flex;align-items:center;gap:8px;margin-bottom:6px;';

    const fsTitle = document.createElement('span');
    fsTitle.textContent = 'MEMFS:';
    fsTitle.style.cssText = 'color:#569cd6;font-size:12px;';

    const fsPath = document.createElement('span');
    fsPath.textContent = fsBrowserPath;
    fsPath.style.cssText = 'color:#888;font-size:12px;flex:1;';

    const fsUploadBtn = document.createElement('button');
    fsUploadBtn.textContent = 'Upload';
    fsUploadBtn.style.cssText = 'background:#3a3a3a;border:1px solid #555;color:#d4d4d4;padding:1px 8px;font-size:11px;';
    fsUploadBtn.addEventListener('click', () => fsUploadInput.click());

    const fsUploadZipBtn = document.createElement('button');
    fsUploadZipBtn.textContent = 'Upload Zip';
    fsUploadZipBtn.style.cssText = 'background:#3a3a3a;border:1px solid #555;color:#d4d4d4;padding:1px 8px;font-size:11px;';
    fsUploadZipBtn.addEventListener('click', () => fsUploadZipInput.click());

    fsHeader.appendChild(fsTitle);
    fsHeader.appendChild(fsPath);
    fsHeader.appendChild(fsUploadBtn);
    fsHeader.appendChild(fsUploadZipBtn);
    rtsFsDiv.appendChild(fsHeader);

    let entries;
    try { entries = lbm.FS.readdir(fsBrowserPath); } catch(e) { entries = []; }

    entries.filter(e => e !== '.' && e !== '..').forEach(name => {
      const fullPath = (fsBrowserPath === '/' ? '' : fsBrowserPath) + '/' + name;
      let isDir = false;
      try { isDir = lbm.FS.isDir(lbm.FS.stat(fullPath).mode); } catch(e) {}

      const row = document.createElement('div');
      row.style.cssText = 'display:flex;align-items:center;justify-content:space-between;padding:2px 4px;font-size:12px;border-bottom:1px solid #1a1a1a;';

      const nameEl = document.createElement('span');
      nameEl.textContent = (isDir ? '\u{1F4C1} ' : '\u{1F4C4} ') + name;
      nameEl.style.cssText = isDir ? 'color:#dcdcaa;cursor:pointer;' : 'color:#d4d4d4;cursor:pointer;';
      if (isDir) {
        nameEl.addEventListener('click', () => { fsBrowserPath = fullPath; refreshFsBrowser(); });
      } else {
        nameEl.addEventListener('dblclick', () => {
          const content = lbm.FS.readFile(fullPath, {encoding: 'utf8'});
          const tab = createEditorTab(name);
          tab.cm.setValue(content);
          tab.filename = name;
        });
      }
      row.appendChild(nameEl);

      const btnWrap = document.createElement('span');
      btnWrap.style.cssText = 'display:flex;gap:4px;';
      const btnStyle = 'background:#3a3a3a;border:1px solid #555;color:#d4d4d4;padding:1px 8px;font-size:11px;cursor:pointer;';

      if (isDir) {
        const zipBtn = document.createElement('button');
        zipBtn.textContent = 'Zip';
        zipBtn.style.cssText = btnStyle;
        zipBtn.addEventListener('click', () => {
          const data = memfsZipDir(fullPath);
          const blob = new Blob([data], {type: 'application/zip'});
          const url  = URL.createObjectURL(blob);
          const a    = document.createElement('a');
          a.href = url; a.download = name + '.zip'; a.click();
          URL.revokeObjectURL(url);
        });
        btnWrap.appendChild(zipBtn);
      } else {
        const dlBtn = document.createElement('button');
        dlBtn.textContent = 'Download';
        dlBtn.style.cssText = btnStyle;
        dlBtn.addEventListener('click', () => {
          const data = lbm.FS.readFile(fullPath);
          const blob = new Blob([data], {type: 'application/octet-stream'});
          const url  = URL.createObjectURL(blob);
          const a    = document.createElement('a');
          a.href = url; a.download = name; a.click();
          URL.revokeObjectURL(url);
        });
        btnWrap.appendChild(dlBtn);
      }
      row.appendChild(btnWrap);

      rtsFsDiv.appendChild(row);
    });

    if (fsBrowserPath !== '/') {
      const upRow = document.createElement('div');
      upRow.textContent = '↑ ..';
      upRow.style.cssText = 'color:#888;font-size:12px;cursor:pointer;padding:2px 4px;';
      upRow.addEventListener('click', () => {
        fsBrowserPath = fsBrowserPath.substring(0, fsBrowserPath.lastIndexOf('/')) || '/';
        refreshFsBrowser();
      });
      rtsFsDiv.insertBefore(upRow, fsHeader.nextSibling);
    }
  }

  let fsBrowserPath = '/';
  refreshFsBrowser();

  setInterval(() => {
    if (rtsTabBtn.classList.contains('active')) refreshRTS();
  }, 500);


  window.canvasPutImage = function(canvasId, rgbaPtr, w, h, x, y) {
    const tab = canvasTabs[canvasId];
    if (!tab) return;
    const bytes   = new Uint8ClampedArray(lbm.HEAP8.buffer, rgbaPtr, w * h * 4);
    const imgData = new ImageData(bytes.slice(), w, h);
    tab.ctx.putImageData(imgData, x, y);
  };

  const SERIES_COLORS = ['#4ec9b0', '#569cd6', '#ce9178', '#dcdcaa', '#c586c0', '#f44747', '#b5cea8', '#9cdcfe'];

  window.createMultiPlotTab = function(slotsJson, title) {

    const {id, label, pane, w, h} =  mkPlotTab(title)  
    
    const bufs = JSON.parse(slotsJson);
    let maxLen = 0;
    const yArrays = bufs.map(({ptr, nbytes}) => {
      const nFloat = (nbytes / 4) | 0;
      const ys     = Array.from(new Float32Array(lbm.HEAP8.buffer, ptr, nFloat));
      if (ys.length > maxLen) maxLen = ys.length;
      return ys;
    });
    const xs = Array.from({length: maxLen}, (_, i) => i);

    addPlotToolbar(pane, label, () => ({ xs, yArrays }));

    const series = [{}];
    bufs.forEach((_, i) => {
      series.push({ label: 'series ' + i, stroke: SERIES_COLORS[i % SERIES_COLORS.length], width: 2 });
    });

    new uPlot({
      title:  label,
      width:  w,
      height: h,
      series,
      axes: DARK_AXES,
      scales: { x: { time: false } },
      cursor: { stroke: '#569cd6', width: 1 },
      plugins: [wheelZoomPlugin],
    }, [xs, ...yArrays], pane);
  };

  fetch('libs/index.json')
    .then(r => r.json())
    .then(files => {
      try { lbm.FS.mkdir('/libs'); } catch(e) {}
      const fetches = files.map(f =>
        fetch('libs/' + f)
          .then(r => r.arrayBuffer())
          .then(buf => lbm.FS.writeFile('/libs/' + f, new Uint8Array(buf)))
      );
      Promise.all(fetches).then(() => refreshFsBrowser());
    });

  console.log('calling lbm_wasm_init...');
  const ok = lbm.ccall('lbm_wasm_init', 'number', [], []);
  console.log('lbm_wasm_init returned:', ok);
  if (!ok) {
    appendOutput('Failed to initialise LispBM.\n');
    statusText.textContent = 'Init failed';
    return;
  }

  btnLoad.disabled = false;
  consoleInput.disabled = false;
  consoleInput.focus();
  statusText.textContent = 'Activity';
  document.querySelector('#output-tab-bar .tab-btn[data-tab="console"]')
    .addEventListener('click', () => consoleInput.focus());

  const fsUploadInput = document.createElement('input');
  fsUploadInput.type = 'file';
  fsUploadInput.multiple = true;
  fsUploadInput.style.display = 'none';
  document.body.appendChild(fsUploadInput);
  fsUploadInput.addEventListener('change', () => {
    const files = Array.from(fsUploadInput.files);
    if (!files.length) return;
    let done = 0;
    files.forEach(file => {
      const reader = new FileReader();
      reader.onload = e => {
        const data = new Uint8Array(e.target.result);
        const dest = (fsBrowserPath === '/' ? '' : fsBrowserPath) + '/' + file.name;
        lbm.FS.writeFile(dest, data);
        appendOutput('Uploaded "' + file.name + '" to MEMFS ' + dest + ' (' + data.length + ' bytes)\n');
        if (++done === files.length) refreshFsBrowser();
      };
      reader.readAsArrayBuffer(file);
    });
    fsUploadInput.value = '';
  });

  const fsUploadZipInput = document.createElement('input');
  fsUploadZipInput.type = 'file';
  fsUploadZipInput.accept = '.zip';
  fsUploadZipInput.style.display = 'none';
  document.body.appendChild(fsUploadZipInput);
  fsUploadZipInput.addEventListener('change', () => {
    const file = fsUploadZipInput.files[0];
    if (!file) return;
    const reader = new FileReader();
    reader.onload = e => {
      try {
        const dirName = file.name.replace(/\.zip$/i, '');
        const dest    = (fsBrowserPath === '/' ? '' : fsBrowserPath) + '/' + dirName;
        try { lbm.FS.mkdir(dest); } catch(e) {}
        memfsUnzipInto(e.target.result, dest);
        appendOutput('Unzipped "' + file.name + '" into ' + dest + '\n');
        refreshFsBrowser();
      } catch(err) {
        appendOutput('Error unzipping "' + file.name + '": ' + err.message + '\n');
      }
    };
    reader.readAsArrayBuffer(file);
    fsUploadZipInput.value = '';
  });

  let ledState    = false;
  let lastLedFlip = 0;

  function loop() {
    const now      = performance.now();
    const deadline = now + 8;
    let   anyBusy  = false;
    try {
      while (performance.now() < deadline) {
        const busy = lbm.ccall('lbm_wasm_run', 'number', ['number'], [parseInt(stepsSelect.value)]);
        if (busy) anyBusy = true;
      }
    } catch(e) {
      appendOutput('CRASH in step: ' + e + '\n');
      statusText.textContent = 'Crashed';
      return;
    }
    try {
      pollOutput();
    } catch(e) {
      appendOutput('CRASH in pollOutput: ' + e + '\n');
      statusText.textContent = 'Crashed';
      return;
    }
    if (anyBusy) {
      const t = performance.now();
      if (t - lastLedFlip > 150) {
        ledState    = !ledState;
        lastLedFlip = t;
        busyLed.classList.toggle('on', ledState);
      }
    } else {
      ledState = false;
      busyLed.classList.remove('on');
    }
    setTimeout(loop, 0);
  }
  setTimeout(loop, 0);

  function evalExpr() {
    const code = consoleInput.value;
    consoleHistory.textContent += '# ' + code + '\n';
    consoleInput.value = '';
    consoleInputDisplay.textContent = '';
    consolePane.scrollTop = consolePane.scrollHeight;
    if (code.trim()) lbm.ccall('lbm_wasm_eval', null, ['string'], [code]);
  }

  function loadEditor() {
    if (!activeEditor || activeEditor.isSim) return;
    const code = activeEditor.cm.getValue().trim();
    if (!code) return;
    window.currentBaseUrl = activeEditor.baseUrl || null;
    lbm.ccall('lbm_wasm_eval_program', null, ['string'], [code]);
  }

  document.getElementById('btn-save').addEventListener('click', () => {
    if (!activeEditor || activeEditor.isSim) return;
    window.openFsDialog({ mode: 'save', title: 'Save to MEMFS', filename: activeEditor.filename || '' }).then(path => {
      if (!path) return;
      try {
        lbm.FS.writeFile(path, activeEditor.cm.getValue());
        const name = path.split('/').pop();
        activeEditor.filename = name;
        activeEditor.labelEl.textContent = name;
        refreshFsBrowser();
      } catch(e) { appendOutput('Error saving ' + path + ': ' + e.message + '\n'); }
    });
  });

  consoleInput.addEventListener('keydown', e => {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      evalExpr();
    }
  });
  btnLoad.addEventListener('click', loadEditor);

}).catch(err => {
  document.getElementById('status').textContent = 'Error: ' + err;
});
