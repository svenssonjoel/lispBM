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

#include "QLbmLabelWidget.h"

#include <QLabel>
#include <QVBoxLayout>

QLbmLabelWidget::QLbmLabelWidget(const QString &text, QWidget *parent)
  : QLbmWidget(parent)
  , m_label(new QLabel(text, this)) {
  auto *layout = new QVBoxLayout(this);
  layout->setContentsMargins(0, 0, 0, 0);
  layout->addWidget(m_label);
  setLayout(layout);
}

QString QLbmLabelWidget::getText() const  { return m_label->text(); }
void    QLbmLabelWidget::setText(const QString &text) { m_label->setText(text); }
