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

#ifndef QLBMSLIDERWIDGET_H_
#define QLBMSLIDERWIDGET_H_

#include "QLbmWidget.h"
#include <Qt>

class QSlider;

class QLbmSliderWidget : public QLbmWidget {
  Q_OBJECT
public:
  explicit QLbmSliderWidget(int min, int max, Qt::Orientation orientation,
                            int handle, QWidget *parent = nullptr);

  int  getValue() const;
  void setValue(int val);

signals:
  void changed(int handle, int value);

private:
  QSlider *m_slider;
  int      m_handle;
};

#endif
