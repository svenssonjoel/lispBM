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

#ifndef QLBMSPINBOXFWIDGET_H_
#define QLBMSPINBOXFWIDGET_H_

#include "QLbmWidget.h"

class QDoubleSpinBox;

// Float spin box. Arrow steps fire committed() immediately; typed entry
// fires committed() only on Enter or focus-loss.
// getValue/setValue use double internally; the LBM extension exposes float.
class QLbmSpinboxFWidget : public QLbmWidget {
  Q_OBJECT
public:
  explicit QLbmSpinboxFWidget(double min, double max, double step,
                               int handle, QWidget *parent = nullptr);

  double getValue() const;
  void   setValue(double val);

signals:
  void committed(int handle, double value);

private:
  QDoubleSpinBox *m_spinbox;
  int             m_handle;
};

#endif
