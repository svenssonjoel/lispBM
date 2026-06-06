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

#ifndef QLBMRADIOWIDGET_H_
#define QLBMRADIOWIDGET_H_

#include "QLbmWidget.h"

class QRadioButton;

// Radio buttons added to the same QLbmContainerWidget are mutually exclusive.
// The container manages a QButtonGroup for this purpose.
class QLbmRadioWidget : public QLbmWidget {
  Q_OBJECT
public:
  explicit QLbmRadioWidget(const QString &label, int handle, QWidget *parent = nullptr);

  bool         getValue() const;
  void         setValue(bool checked);
  QRadioButton *radioButton() const { return m_radio; }

signals:
  void selected(int handle);

private:
  QRadioButton *m_radio;
  int           m_handle;
};

#endif
