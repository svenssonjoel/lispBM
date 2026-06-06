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

#ifndef QLBMTEXTFIELDWIDGET_H_
#define QLBMTEXTFIELDWIDGET_H_

#include "QLbmWidget.h"
#include <QString>

class QLineEdit;

// Single-line text input. committed() fires only on Enter or focus-loss,
// not on every keystroke.
class QLbmTextfieldWidget : public QLbmWidget {
  Q_OBJECT
public:
  explicit QLbmTextfieldWidget(const QString &placeholder, int handle,
                                QWidget *parent = nullptr);

  QString getValue() const;
  void    setValue(const QString &text);

signals:
  void committed(int handle, const QString &text);

private:
  QLineEdit *m_lineEdit;
  int        m_handle;
};

#endif
