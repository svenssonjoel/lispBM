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

#ifndef MAINWINDOW_H_
#define MAINWINDOW_H_

#include <QMainWindow>

class QPlainTextEdit;
class QPushButton;
class QLispBM;
class QLbmContainerWidget;

class MainWindow : public QMainWindow {
  Q_OBJECT
public:
  explicit MainWindow(QWidget *parent = nullptr);
  ~MainWindow();

private slots:
  void onEvaluate();
  void onLbmOutput(const QString &text);
  void onEvalFinished(int cid, const QString &result);
  void onEvalFailed(int cid, const QString &error);

private:
  QPlainTextEdit *m_editor;
  QPlainTextEdit *m_output;
  QLbmContainerWidget *m_lbmWidget;
  QLispBM        *m_lbm;
};

#endif
