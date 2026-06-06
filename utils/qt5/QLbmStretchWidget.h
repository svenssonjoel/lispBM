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

#ifndef QLBMSTRETCHWIDGET_H_
#define QLBMSTRETCHWIDGET_H_

#include "QLbmWidget.h"
#include <Qt>

// A transparent expanding spacer widget. When added to a VBox container it
// absorbs vertical space; in an HBox it absorbs horizontal space.
class QLbmStretchWidget : public QLbmWidget {
  Q_OBJECT
public:
  explicit QLbmStretchWidget(Qt::Orientation orientation,
                              QWidget *parent = nullptr);
};

#endif
