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

#include "QLbmSliderWidget.h"

#include <QSlider>
#include <QVBoxLayout>

QLbmSliderWidget::QLbmSliderWidget(int min, int max, Qt::Orientation orientation,
                                   int handle, QWidget *parent)
  : QLbmWidget(parent)
  , m_handle(handle) {
  auto *layout = new QVBoxLayout(this);
  layout->setContentsMargins(0, 0, 0, 0);
  m_slider = new QSlider(orientation, this);
  m_slider->setRange(min, max);
  layout->addWidget(m_slider);
  setLayout(layout);

  connect(m_slider, &QSlider::valueChanged, this, [this](int val) {
    emit changed(m_handle, val);
  });
}

int  QLbmSliderWidget::getValue() const  { return m_slider->value(); }
void QLbmSliderWidget::setValue(int val) { m_slider->setValue(val); }
