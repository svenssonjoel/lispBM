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

#include "QLbmSpinboxFWidget.h"

#include <QDoubleSpinBox>
#include <QVBoxLayout>

QLbmSpinboxFWidget::QLbmSpinboxFWidget(double min, double max, double step,
                                         int handle, QWidget *parent)
  : QLbmWidget(parent)
  , m_handle(handle) {
  auto *layout = new QVBoxLayout(this);
  layout->setContentsMargins(0, 0, 0, 0);
  m_spinbox = new QDoubleSpinBox(this);
  m_spinbox->setRange(min, max);
  m_spinbox->setSingleStep(step);
  m_spinbox->setDecimals(3);
  m_spinbox->setKeyboardTracking(false);
  layout->addWidget(m_spinbox);
  setLayout(layout);

  connect(m_spinbox, QOverload<double>::of(&QDoubleSpinBox::valueChanged), this, [this](double val) {
    emit committed(m_handle, val);
  });
}

double QLbmSpinboxFWidget::getValue() const    { return m_spinbox->value(); }
void   QLbmSpinboxFWidget::setValue(double val) { m_spinbox->setValue(val); }
