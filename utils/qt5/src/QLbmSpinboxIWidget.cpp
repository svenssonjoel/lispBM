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

#include "QLbmSpinboxIWidget.h"

#include <QSpinBox>
#include <QVBoxLayout>

QLbmSpinboxIWidget::QLbmSpinboxIWidget(int min, int max, int handle, QWidget *parent)
  : QLbmWidget(parent)
  , m_handle(handle) {
  auto *layout = new QVBoxLayout(this);
  layout->setContentsMargins(0, 0, 0, 0);
  m_spinbox = new QSpinBox(this);
  m_spinbox->setRange(min, max);
  // With keyboardTracking off, valueChanged fires immediately for arrow steps
  // but only on Enter/focus-loss for typed input.
  m_spinbox->setKeyboardTracking(false);
  layout->addWidget(m_spinbox);
  setLayout(layout);

  connect(m_spinbox, QOverload<int>::of(&QSpinBox::valueChanged), this, [this](int val) {
    emit committed(m_handle, val);
  });
}

int  QLbmSpinboxIWidget::getValue() const  { return m_spinbox->value(); }
void QLbmSpinboxIWidget::setValue(int val) { m_spinbox->setValue(val); }
