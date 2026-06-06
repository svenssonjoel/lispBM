/*
  Copyright 2026 Joel Svensson  svenssonjoel@yahoo.se

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "MainWindow.h"

#include "QLispBM.h"
#include "QLbmContainerWidget.h"

extern "C" {
#include "lispbm.h"
#include "eval_cps.h"
}

static lbm_value ext_print(lbm_value *args, lbm_uint argn) {
  char buf[256];
  for (lbm_uint i = 0; i < argn; i++) {
    lbm_print_value(buf, sizeof(buf), args[i]);
    lbm_printf_callback("%s%s", buf, i < argn - 1 ? " " : "");
  }
  lbm_printf_callback("\n");
  return ENC_SYM_TRUE;
}

#include <QPlainTextEdit>
#include <QPushButton>
#include <QSplitter>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QWidget>
#include <QFont>
#include <QLabel>

MainWindow::MainWindow(QWidget *parent)
  : QMainWindow(parent) {
  setWindowTitle("LispBM Editor");
  resize(1024, 700);

  // --- LispBM runtime ---
  m_lbm = new QLispBM(this);

  m_lbmWidget = new QLbmContainerWidget(this);
  m_lbm->setWidget(m_lbmWidget);
  m_lbm->init();
  m_lbm->addExtension("print", ext_print);
  m_lbm->start();

  connect(m_lbm, &QLispBM::output,        this, &MainWindow::onLbmOutput);
  connect(m_lbm, &QLispBM::evalFinished,  this, &MainWindow::onEvalFinished);
  connect(m_lbm, &QLispBM::evalFailed,    this, &MainWindow::onEvalFailed);

  // --- Editor ---
  m_editor = new QPlainTextEdit(this);
  QFont editorFont("Monospace");
  editorFont.setStyleHint(QFont::Monospace);
  editorFont.setPointSize(11);
  m_editor->setFont(editorFont);
  m_editor->setPlaceholderText("Enter LispBM code here...");

  // --- LBM widget panel (labelled) ---
  auto *lbmPanel = new QWidget(this);
  auto *lbmPanelLayout = new QVBoxLayout(lbmPanel);
  lbmPanelLayout->setContentsMargins(0, 0, 0, 0);
  lbmPanelLayout->addWidget(new QLabel("LispBM UI", lbmPanel));
  lbmPanelLayout->addWidget(m_lbmWidget, 1);
  lbmPanel->setMinimumWidth(200);

  // --- Horizontal splitter: editor | lbm widget ---
  auto *hSplitter = new QSplitter(Qt::Horizontal, this);
  hSplitter->addWidget(m_editor);
  hSplitter->addWidget(lbmPanel);
  hSplitter->setStretchFactor(0, 3);
  hSplitter->setStretchFactor(1, 2);

  // --- Output area ---
  m_output = new QPlainTextEdit(this);
  m_output->setReadOnly(true);
  m_output->setMaximumHeight(160);
  QFont outputFont("Monospace");
  outputFont.setStyleHint(QFont::Monospace);
  outputFont.setPointSize(10);
  m_output->setFont(outputFont);
  m_output->setPlaceholderText("Output...");

  // --- Evaluate button ---
  auto *evalButton = new QPushButton("Evaluate", this);
  evalButton->setShortcut(Qt::CTRL | Qt::Key_Return);
  connect(evalButton, &QPushButton::clicked, this, &MainWindow::onEvaluate);

  auto *clearButton = new QPushButton("Clear Output", this);
  connect(clearButton, &QPushButton::clicked, m_output, &QPlainTextEdit::clear);

  auto *buttonBar = new QWidget(this);
  auto *buttonLayout = new QHBoxLayout(buttonBar);
  buttonLayout->setContentsMargins(0, 0, 0, 0);
  buttonLayout->addWidget(evalButton);
  buttonLayout->addWidget(clearButton);
  buttonLayout->addStretch();

  // --- Vertical splitter: top (editor+lbm) | output ---
  auto *vSplitter = new QSplitter(Qt::Vertical, this);
  vSplitter->addWidget(hSplitter);
  vSplitter->addWidget(m_output);
  vSplitter->setStretchFactor(0, 3);
  vSplitter->setStretchFactor(1, 1);

  // --- Central widget ---
  auto *central = new QWidget(this);
  auto *centralLayout = new QVBoxLayout(central);
  centralLayout->addWidget(buttonBar);
  centralLayout->addWidget(vSplitter, 1);
  setCentralWidget(central);
}

MainWindow::~MainWindow() {
  m_lbm->terminate();
}

void MainWindow::onEvaluate() {
  QString code = m_editor->toPlainText().trimmed();
  if (!code.isEmpty()) {
    m_lbm->evalProgram(code);
  }
}

void MainWindow::onLbmOutput(const QString &text) {
  m_output->moveCursor(QTextCursor::End);
  m_output->insertPlainText(text);
}

void MainWindow::onEvalFinished(int cid, const QString &result) {
  (void)cid;
  m_output->appendPlainText(QString("> %1").arg(result));
}

void MainWindow::onEvalFailed(int cid, const QString &error) {
  (void)cid;
  m_output->appendPlainText(QString("ERROR: %1").arg(error));
}
